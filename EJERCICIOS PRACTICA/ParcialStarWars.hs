import Text.Show.Functions ()
import Data.List (genericLength)


--PUNTO UNO

data Nave = Nave {
    nombre :: String,
    duracion :: Int,
    escudo :: Int,
    ataque :: Int,
    poder :: Poder
} deriving (Show)

type Poder = Nave -> Nave

--Definiciones de naves
naveTie :: Nave
naveTie = Nave "TIE Fighter" 200 100 50 movimientoTurbo

naveXWing :: Nave
naveXWing = Nave "X Wing" 300 150 100 reparacionEmergencia

naveDarth :: Nave
naveDarth = Nave "Nave de Darth Vader" 500 300 200 superTurbo

naveMillenium :: Nave
naveMillenium = Nave "Millennium Falcon" 1000 500 50 (reparacionEmergencia.modificarEscudo 100)

--Poderes y modificaciones
movimientoTurbo :: Poder
movimientoTurbo = modificarAtaque 25

modificarAtaque :: Int -> Poder
modificarAtaque cantidad unaNave = unaNave {ataque= max 0 (ataque unaNave + cantidad)}

reparacionEmergencia :: Poder
reparacionEmergencia = (modificarDurabilidad 50).(modificarAtaque (-30))

modificarDurabilidad :: Int -> Poder
modificarDurabilidad cantidad unaNave = unaNave {duracion= max 0 (duracion unaNave + cantidad)}

superTurbo :: Poder
superTurbo unaNave =  (modificarDurabilidad (-45)) . tripleMovimientoTurbo $ unaNave

tripleMovimientoTurbo :: Poder
tripleMovimientoTurbo unaNave = foldr ($) unaNave (take 3 (repeat movimientoTurbo)) 


modificarEscudo :: Int -> Poder
modificarEscudo cantidad unaNave = unaNave {escudo= max 0 (escudo unaNave + cantidad)}

--Nave agregada
navePrueba :: Nave
navePrueba = Nave "Nave de Prueba" 1000 500 250 (reparacionEmergencia.movimientoTurbo)

--PUNTO DOS
durabilidadFlota :: Flota -> Int
durabilidadFlota naves = sum (map duracion naves)

--PUNTO TRES
atacarNave :: Nave -> Nave -> Nave
atacarNave atacante defensor
    |escudo defensorActivado > ataque atacanteActivado = defensorActivado
    |otherwise = ejecutarAtaque atacanteActivado defensorActivado
    where   atacanteActivado = activarPoder atacante
            defensorActivado = activarPoder defensor

activarPoder :: Nave -> Nave
activarPoder unaNave = (poder unaNave) unaNave

ejecutarAtaque :: Nave -> Nave -> Nave
ejecutarAtaque atacante defensor = modificarEscudo (-((escudo defensor) - (ataque atacante))) defensor

--PUNTO CUATRO
naveFueraDeCombate :: Nave -> Bool
naveFueraDeCombate unaNave = duracion unaNave < 0

--PUNTO CINCO
type Flota = [Nave]
type Estrategia = Nave -> Bool

naveDebil :: Estrategia
naveDebil naveEnemiga = escudo naveEnemiga < 200

navePeligrosa :: Int -> Estrategia
navePeligrosa valorAtaque naveEnemiga = ataque naveEnemiga > valorAtaque

navePosibleFueraCombate :: Nave -> Estrategia
navePosibleFueraCombate naveAtacante naveEnemiga = naveFueraDeCombate . atacarNave naveAtacante $ naveEnemiga

naveTocada :: Estrategia
naveTocada naveEnemiga = duracion naveEnemiga < 100

misionSorpresa :: Estrategia -> Flota -> Nave -> Flota
misionSorpresa estrategia flota naveAtacante = atacarFlota . filter (estrategia) $ flota
    where atacarFlota = map (atacarNave naveAtacante)

--PUNTO SEIS
compararEstrategias :: Estrategia -> Estrategia -> Flota -> Nave -> Flota
compararEstrategias estrategia1 estrategia2 flota naveAtacante
    |durabilidadFlota (misionEstrategia1) > durabilidadFlota (misionEstrategia2) = misionEstrategia2
    |otherwise = misionEstrategia1
        where   misionEstrategia1 = misionSorpresa estrategia1 flota naveAtacante
                misionEstrategia2 = misionSorpresa estrategia2 flota naveAtacante

--PUNTO SIETE
flotaInfinita :: Flota
flotaInfinita = [naveTie, naveDarth] ++ cycle [naveXWing, naveMillenium]

--No es posible determinar su durabilidad total debido a que la funcion map en la durabilidad flota nunca va a finalizar o arrojar un valor para poder hacer la sum (que al ser infinito tampoco podria)
--En este caso, primero va a filtrar y ejecutar el ataque sobre nave Tie correctamente, pero luego va a ciclar sobre naveXWing haciendo siempre un ataque sobre estas mismas naves infinita
--Haskell utiliza lazy evaluation, por lo que si esta lista infinita se ingresara a una funcion que requiera solo una condicion Bool por ejemplo, se va a arrojar una respuesta cuando el ciclo infinito la encuentre, y no quedara ciclando.
