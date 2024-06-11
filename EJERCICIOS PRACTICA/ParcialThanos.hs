import Text.Show.Functions ()
import Data.List ()

--PARCIAL THANOS

---------------
-- PUNTO UNO --
---------------

data Guantelete = Guantelete{
    material :: String,
    gemas :: [Gema]
} deriving (Show)

type Gema = Personaje -> Personaje 

data Personaje = Personaje{
    edad :: Int,
    energia :: Int,
    habilidades :: [Habilidad],
    nombre :: String,
    planeta :: String
} deriving (Show)

type Habilidad = String

type Universo = [Personaje]

esGuanteCompleto :: Guantelete -> Bool
esGuanteCompleto guantelete = 
    (length.gemas) guantelete == 6 &&
    (material guantelete) == "Uru"

chasquido :: Guantelete -> Universo -> Universo
chasquido guante personajes
    |esGuanteCompleto guante = eliminarMitad personajes
    |otherwise = personajes

eliminarMitad :: Universo -> Universo
eliminarMitad personajes = take (length personajes `div` 2) personajes

---------------
-- PUNTO DOS --
---------------

esAptoPendex :: Universo -> Bool
esAptoPendex = any ((<45).edad)

energiaTotal :: Universo -> Int
energiaTotal personajes = sum (map energia (filtrarCantidadHabilidad 1 personajes))

filtrarCantidadHabilidad :: Int -> Universo -> Universo
filtrarCantidadHabilidad cantidad personajes = filter ((>=cantidad).length.habilidades) personajes

----------------
-- PUNTO TRES --
----------------

--Gema 1
laMente :: Int -> Gema
laMente debilitacion oponente = modificarEnergia (-debilitacion) oponente

modificarEnergia :: Int -> Personaje -> Personaje
modificarEnergia cantidad oponente = oponente {energia = energia oponente + cantidad}

--Gema 2
elAlma :: String -> Gema
elAlma habilidad oponente
    |tieneHabilidad habilidad oponente = (eliminarHabilidad habilidad).(modificarEnergia (-10)) $ oponente
    |otherwise = modificarEnergia (-10) oponente

tieneHabilidad :: String -> Personaje -> Bool
tieneHabilidad habilidad oponente = elem habilidad (habilidades oponente)

eliminarHabilidad :: String -> Personaje -> Personaje
eliminarHabilidad habilidad oponente = oponente {habilidades = filter (/=habilidad) (habilidades oponente)}

--Gema 3
elEspacio :: String -> Gema
elEspacio planeta oponente = (modificarPlaneta planeta) . (modificarEnergia (-20)) $ oponente

modificarPlaneta :: String -> Personaje -> Personaje
modificarPlaneta planeta oponente= oponente {planeta=planeta}

--Gema 4
elPoder :: Gema
elPoder oponente = (modificarEnergia (-(energia oponente)) ) . (quitarHabilidades) $ oponente

quitarHabilidades :: Personaje -> Personaje
quitarHabilidades oponente
    |cantidadHabilidades <= 3 = oponente{habilidades= drop (cantidadHabilidades) (habilidades oponente)}
    |otherwise = oponente
    where cantidadHabilidades = length (habilidades oponente)

--Gema 5
elTiempo :: Gema
elTiempo oponente = (modificarEnergia (-50)) . (dividirEdad) $ oponente 

dividirEdad :: Personaje -> Personaje
dividirEdad oponente = oponente {edad= max 18 ((edad oponente) `div` 2)}

--Gema 6
laLoca :: Gema -> Gema
laLoca gema oponente = foldr ($) oponente (take 2 (repeat gema)) 

------------------
-- PUNTO CUATRO --
------------------

guanteleteEjemplo :: Guantelete
guanteleteEjemplo = Guantelete "Goma" [elTiempo, elAlma "usar Mjolnir", laLoca (elAlma "programacion en Haskell")]

------------------
-- PUNTO CINCO --
------------------

utilizarGemas :: [Gema] -> Personaje -> Personaje
utilizarGemas gemas oponente= foldr ($) oponente (gemas)

----------------
-- PUNTO SEIS --
----------------

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa guante oponente = elegirPoderosa (gemas guante) oponente

elegirPoderosa :: [Gema] -> Personaje -> Gema
elegirPoderosa [x] _ = x
elegirPoderosa (x:y:xs) personaje 
    |energia (x personaje) < energia (y personaje) = elegirPoderosa (y:xs) personaje
    |otherwise= elegirPoderosa (x:xs) personaje

-----------------
-- PUNTO SIETE --
-----------------
--La funcion gema mas poderosa no podre usarla porque va a comparar infinitamente
--la funcion usoLasTresPrimeras si puedo, debido a que solo toma las primeras 3 gemas despreciando todas las demas, no evalua al pedo

