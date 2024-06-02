import Text.Show.Functions ()
import Data.List ()

--PUNTO UNO
--Modelar a los heroes

data Heroe = Heroe {
    epiteto        :: String,
    reconocimiento :: Int,
    artefactos     :: [Artefacto],
    tareas         :: [Tarea]
} deriving (Show)

data Artefacto = Artefacto {
    nombre :: String,
    rareza :: Int
} deriving (Show)

--PUNTO DOS

pasarALaHistoria :: Heroe -> Heroe
pasarALaHistoria unHeroe  
    |reconocimiento unHeroe > 1000 = cambiarEpiteto "El mitico" unHeroe
    |reconocimiento unHeroe >= 500 = cambiarEpiteto "El magnifico" . agregarArtefacto lanzaDelOlimpo $ unHeroe
    |reconocimiento unHeroe > 100 = cambiarEpiteto "Hoplita" . agregarArtefacto xiphos $ unHeroe
    |otherwise = unHeroe

cambiarEpiteto ::  String -> Heroe -> Heroe
cambiarEpiteto epitetoNuevo unHeroe= unHeroe {epiteto = epitetoNuevo}

agregarArtefacto ::  Artefacto -> Heroe -> Heroe
agregarArtefacto unArtefacto unHeroe = unHeroe {artefactos = unArtefacto : artefactos unHeroe} --el : suma el elemento a la lista

lanzaDelOlimpo :: Artefacto
lanzaDelOlimpo = Artefacto "Lanza Del Olimpo" 100

xiphos :: Artefacto
xiphos = Artefacto "Xiphos" 50

relampagoZeus :: Artefacto
relampagoZeus = Artefacto "Relampago de Zeus" 500

--PUNTO TRES

type Tarea = Heroe -> Heroe

--Tarea encontrar artefacto
encontrarArtefacto :: Artefacto -> Tarea
encontrarArtefacto unArtefacto = ganarRecocimiento unArtefacto . agregarArtefacto unArtefacto

ganarRecocimiento :: Artefacto -> Heroe -> Heroe
ganarRecocimiento unArtefacto unHeroe = unHeroe {reconocimiento = reconocimiento unHeroe + rareza unArtefacto}

--Tarea Escalar Olimpo
escalarOlimpo :: Tarea
escalarOlimpo = agregarArtefacto relampagoZeus . agregarReconocimiento 500 . triplicarRarezaArtefactos . desecharArtefactosComunes

agregarReconocimiento ::  Int -> Heroe -> Heroe
agregarReconocimiento reconocimientoExtra unHeroe  = unHeroe {reconocimiento= reconocimientoExtra + reconocimiento unHeroe }

triplicarRarezaArtefactos :: Tarea
triplicarRarezaArtefactos = cambiarArtefactos (map triplicarRarezaArtefacto)

triplicarRarezaArtefacto :: Artefacto -> Artefacto
triplicarRarezaArtefacto unArtefacto = unArtefacto {rareza = (*3) . rareza $ unArtefacto}

desecharArtefactosComunes :: Tarea
desecharArtefactosComunes = cambiarArtefactos (filter (not . esComun))

esComun :: Artefacto -> Bool
esComun unArtefacto = rareza unArtefacto > 1000

cambiarArtefactos :: ([Artefacto] -> [Artefacto]) -> Heroe -> Heroe 
cambiarArtefactos modificador unHeroe = unHeroe {artefactos = modificador (artefactos unHeroe)}

--tarea cruzar la calle
ayudarCruzarCalle :: Int -> Tarea
ayudarCruzarCalle cantidadVeces = cambiarEpiteto ("Gros" ++ replicate cantidadVeces 'o')

--tareaMatarBestia
matarUnaBestia :: Bestia -> Tarea
matarUnaBestia unaBestia unHeroe
    | (debilidad unaBestia) unHeroe = cambiarEpiteto ("El asesino de " ++ nombreBestia unaBestia) unHeroe
    | otherwise                     = cambiarEpiteto "El cobarde" . cambiarArtefactos (drop 1) $ unHeroe

data Bestia = Bestia {
    nombreBestia :: String,
    debilidad    :: Debilidad
} deriving (Show)

type Debilidad = Heroe -> Bool

--PUNTO 4
--modelar a eracles
eracles :: Heroe
eracles = Heroe "Guardian Del Olimpo" 700 [pistola, relampagoZeus] [matarUnaBestia leonNemea]

pistola :: Artefacto
pistola = Artefacto "Fierro de Grecia" 1000

--PUNTO 5
--modelar tarea matar leon de nemea
leonNemea :: Bestia
leonNemea = Bestia "Leon de Nemea" ((> 20) . length . epiteto)

--PUNTO 6
--hacer que un heroe haga una tarea
hacerUnaTarea :: Tarea -> Heroe -> Heroe
hacerUnaTarea unaTarea unHeroe = agregarTarea unaTarea (unaTarea unHeroe)

agregarTarea :: Tarea -> Heroe -> Heroe
agregarTarea unaTarea unHeroe = unHeroe {tareas = unaTarea : tareas unHeroe}

--PUNTO 7
presumirHeroes :: Heroe -> Heroe -> (Heroe, Heroe)
presumirHeroes unHeroe otroHeroe
    |esGanador unHeroe otroHeroe = (unHeroe, otroHeroe)
    |esGanador otroHeroe unHeroe = (otroHeroe, unHeroe)
    |otherwise = presumirHeroes (realizarLabor (tareas otroHeroe) unHeroe) (realizarLabor (tareas unHeroe) otroHeroe)

esGanador :: Heroe -> Heroe -> Bool
esGanador unHeroe otroHeroe = 
    reconocimiento unHeroe > reconocimiento otroHeroe ||
    reconocimiento unHeroe == reconocimiento otroHeroe && sumaRareza unHeroe > sumaRareza otroHeroe

sumaRareza :: Heroe -> Int
sumaRareza unHeroe = sum (map rareza (artefactos unHeroe))

--PUNTO 8
{-Cuál es el resultado de hacer que presuman dos héroes con reconocimiento 100, ningún artefacto y ninguna tarea realizada?
El resultado es que al tener el mismo reconocimiento, y el mismo nivel de artefactos, entrara al otherwise que va a compararlos relizando tareas
pero como no tiene ninguna tarea para que pueda modificarse algun valor y arrojar un ganador, sera un loop infinito que no arroja resultado-}

--PUNTO 9
realizarLabor :: [Tarea] -> Heroe -> Heroe
realizarLabor tareas unHeroe =  foldl (flip hacerUnaTarea) unHeroe tareas

--PUNTO 10
{-Si invocamos la función anterior con una labor infinita, ¿se podrá conocer el estado final del héroe? ¿Por qué?
haskell aplica Lazy Evaluation, es decir que evalua hasta obtener el valor necesario para cumplir la funcion, pero en este caso
la funcion pide realizar una tarea infinita, por lo que haskell realizara todas las tareas sin llegar a un final.
-}