import Data.Time.Clock
import Data.Time.Format ( formatTime, defaultTimeLocale )
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq)
import Data.List

data Vehiculo = Vehiculo {
    placa :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime 
} deriving (Show, Read)

registrarEntrada :: String -> UTCTime -> [Vehiculo] -> [Vehiculo]
registrarEntrada placaV tiempo parqueadero =
    if verificarVehiculoEnParqueadero placaV parqueadero
    then parqueadero
    else Vehiculo placaV tiempo Nothing : parqueadero

registrarSalida :: String -> UTCTime -> [Vehiculo] -> [Vehiculo]
registrarSalida placaV tiempo parqueadero =
    map (\v -> if placaV == placa v then v { salida = Just tiempo } else v) parqueadero

buscarVehiculo :: String -> [Vehiculo] -> Maybe Vehiculo
buscarVehiculo placaV parqueadero =
    find (\v -> placaV == placa v && isNothing (salida v)) parqueadero
    where
        isNothing Nothing = True
        isNothing _       = False

verificarVehiculoEnParqueadero :: String -> [Vehiculo] -> Bool
verificarVehiculoEnParqueadero placaV parqueadero =
    case buscarVehiculo placaV parqueadero of
        Just _  -> True
        Nothing -> False

tiempoEnParqueadero :: Vehiculo -> UTCTime -> NominalDiffTime
tiempoEnParqueadero vehiculo tiempoActual =
    diffUTCTime tiempoActual (entrada vehiculo)

-- Funciones de visualización y manipulación de archivos
mostrarVehiculo :: Vehiculo -> String
mostrarVehiculo (Vehiculo placa entrada (Just salida)) =
    placa ++ "," ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" entrada ++ "," ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" salida
mostrarVehiculo (Vehiculo placa entrada Nothing) =
    placa ++ "," ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" entrada ++ ","

guardarParqueadero :: [Vehiculo] -> IO ()
guardarParqueadero parqueadero = do
    withFile "parqueadero.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarVehiculo parqueadero))
    putStrLn "Parqueadero guardado en el archivo parqueadero.txt."

cargarParqueadero :: IO [Vehiculo]
cargarParqueadero = do
    resultado <- try (readFile "parqueadero.txt") :: IO (Either IOException String) 
    case resultado of
        Left ex -> do
            putStrLn $ "Advertencia: No se pudo leer el archivo parqueadero.txt: " ++ show ex
            return []
        Right contenido -> do
            contenido `deepseq` return ()  
            let parqueadero = map leerVehiculo (lines contenido)
            return parqueadero

leerVehiculo :: String -> Vehiculo
leerVehiculo linea =
    let [placa, entrada, salida] = split ',' linea
    in Vehiculo placa (read entrada) (if salida == "" then Nothing else Just (read salida))

split :: Char -> String -> [String]
split _ "" = [""]
split delimiter str =
    foldr (\c acc -> if c == delimiter then "":acc else (c:head acc):tail acc) [""] str

eliminarVehiculo :: String -> [Vehiculo] -> [Vehiculo]
eliminarVehiculo placaVehiculo = filter (\v -> placa v /= placaVehiculo)

formatoVehiculo :: Vehiculo -> String
formatoVehiculo vehiculo =
    "Vehiculo {PLACA: " ++ mostrarVehiculo vehiculo ++ "}"

-- Funciones principales del programa
main :: IO ()
main = do
    parqueaderoInicial <- cargarParqueadero
    putStrLn "¡Bienvenido al Sistema de Gestión de Parqueadero!"
    cicloPrincipal parqueaderoInicial

cicloPrincipal :: [Vehiculo] -> IO ()
cicloPrincipal parqueadero = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de vehículo"
    putStrLn "2. Registrar salida de vehículo"
    putStrLn "3. Buscar vehículo por placa"
    putStrLn "4. Mostrar vehiculos"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la placa del vehículo:"
            placaVehiculo <- getLine
            if verificarVehiculoEnParqueadero placaVehiculo parqueadero
            then do
                putStrLn $ "El vehículo con placa " ++ placaVehiculo ++ " ya se encuentra en el parqueadero."
                cicloPrincipal parqueadero
            else do
                tiempoActual <- getCurrentTime
                let parqueaderoActualizado = registrarEntrada placaVehiculo tiempoActual parqueadero
                putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " ingresado al parqueadero."
                guardarParqueadero parqueaderoActualizado
                cicloPrincipal parqueaderoActualizado

        "2" -> do
            putStrLn "Ingrese la placa del vehículo a salir:"
            placaVehiculo <- getLine
            case buscarVehiculo placaVehiculo parqueadero of
                Just vehiculo -> do
                    tiempoActual <- getCurrentTime
                    let parqueaderoActualizado = eliminarVehiculo placaVehiculo (registrarSalida placaVehiculo tiempoActual parqueadero)
                    putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " ha salido del parqueadero."
                    guardarParqueadero parqueaderoActualizado
                    cicloPrincipal parqueaderoActualizado
                Nothing -> do
                    putStrLn "Vehículo no encontrado en el parqueadero o ya ha salido."
                    cicloPrincipal parqueadero

        "3" -> do
            putStrLn "Ingrese la placa del vehículo a buscar:"
            placaVehiculo <- getLine
            tiempoActual <- getCurrentTime
            case buscarVehiculo placaVehiculo parqueadero of
                Just vehiculo -> do
                    let tiempoTotal = tiempoEnParqueadero vehiculo tiempoActual
                    putStrLn $ "El vehículo con placa " ++ placaVehiculo ++ " se encuentra en el parqueadero."
                    putStrLn $ "Tiempo en parqueadero: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Vehículo no encontrado en el parqueadero."
            cicloPrincipal parqueadero

        "4" -> do
            contenido <- readFile "parqueadero.txt"
            let lineas  = lines contenido
                vehiculos = map leerVehiculo lineas
                vehiculosFormateados = map formatoVehiculo vehiculos
            mapM_ putStrLn vehiculosFormateados
            cicloPrincipal parqueadero

        "5" -> putStrLn "Saliste del sistema de parqueadero!"

        _ -> do
            putStrLn "Opción no válida."
            cicloPrincipal parqueadero
