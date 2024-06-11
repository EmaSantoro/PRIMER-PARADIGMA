import Text.Show.Functions()

--PARCIAL HEROES DE LEYENDA

--PUNTO UNO, modelado de heroes
data Heroe = Heroe {
    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto],
    tareas :: [Tarea]
} deriving (Show)

type Tarea = Heroe -> Heroe

data Artefacto = Artefacto {
    tipo :: String,
    rareza :: Int
} deriving (Show)


--PUNTO DOS, hacer que un heroe pase a la historia

pasarALaHistoria :: Heroe -> Heroe
pasarALaHistoria unHeroe
    |reconocimiento unHeroe > 1000 = cambiarEpiteto "El mitico" unHeroe
    |reconocimiento unHeroe > 500 = (cambiarEpiteto "El magnifico").(agregarArtefacto lanzaOlimpo) $ unHeroe
    |reconocimiento unHeroe > 100 = (cambiarEpiteto "Hoplitas").(agregarArtefacto xiphos) $ unHeroe
    |otherwise = unHeroe

cambiarEpiteto :: String -> Tarea
cambiarEpiteto nuevoEpiteto unHeroe = unHeroe {epiteto = nuevoEpiteto}

agregarArtefacto :: Artefacto -> Tarea
agregarArtefacto unArtefacto unHeroe = unHeroe {artefactos = unArtefacto : (artefactos unHeroe)}

lanzaOlimpo, xiphos, relampagoZeus :: Artefacto
lanzaOlimpo = Artefacto "Lanza del Olimpo" 100
xiphos = Artefacto "Xiphos" 50
relampagoZeus = Artefacto "El Rempalago de Zeus" 500

--PUNTO TRES, modelar tareas del heroe
--tarea 1
encontrarArtefacto :: Artefacto -> Tarea
encontrarArtefacto unArtefacto unHeroe = (agregarArtefacto unArtefacto).(modificarReconocimiento valorRareza) $ unHeroe
    where valorRareza = rareza unArtefacto

modificarReconocimiento :: Int -> Tarea
modificarReconocimiento valor unHeroe = unHeroe {reconocimiento = reconocimiento unHeroe + valor}

--tarea 2
escalarOlimpo :: Tarea
escalarOlimpo unHeroe = (agregarArtefacto relampagoZeus).(artefactosPostOlimpo).(modificarReconocimiento 500) $ unHeroe

artefactosPostOlimpo :: Tarea
artefactosPostOlimpo unHeroe = unHeroe {artefactos = filtrarComun (triplicarRarezaArtefacto unHeroe)}

triplicarRarezaArtefacto :: Heroe -> [Artefacto]
triplicarRarezaArtefacto unHeroe = map (triplicarRareza) (artefactos unHeroe)

triplicarRareza :: Artefacto -> Artefacto
triplicarRareza unArtefacto = unArtefacto {rareza = (*3) (rareza unArtefacto)}

esComun :: Artefacto -> Bool
esComun unArtefacto = 1000 > rareza unArtefacto

filtrarComun :: [Artefacto] -> [Artefacto]
filtrarComun unosArtefactos = filter esComun unosArtefactos

--tarea 3
ayudarACruzar :: Int -> Tarea
ayudarACruzar crucesCalle unHeroe = cambiarEpiteto ("Gros" ++ (replicate crucesCalle 'o')) unHeroe

--tarea 4 
matarUnaBestia :: Bestia -> Tarea
matarUnaBestia unaBestia unHeroe 
    |(debilidad unaBestia) unHeroe = cambiarEpiteto ("El asesino de " ++ (nombreBestia unaBestia)) unHeroe
    |otherwise= (cambiarEpiteto "El cobarde") . (perderArtefacto 1)$ unHeroe

perderArtefacto :: Int -> Tarea
perderArtefacto cantidad unHeroe = unHeroe {artefactos= drop cantidad (artefactos unHeroe)}

data Bestia = Bestia {
    nombreBestia :: String,
    debilidad :: Debilidad
} deriving (Show)

type Debilidad = Heroe -> Bool

--PUNTO CUATRO
heracles :: Heroe
heracles = Heroe "El Guardian del Olimpo" 700 [pistola, relampagoZeus] [matarLeonNemea]

pistola :: Artefacto
pistola = Artefacto "Fierro Griego" 1000

--PUNTO CINCO
matarLeonNemea :: Tarea
matarLeonNemea unHeroe = matarUnaBestia leonNemea unHeroe

leonNemea :: Bestia
leonNemea = Bestia "Leon De Nemea" epitetoLargo

epitetoLargo :: Debilidad
epitetoLargo unHeroe = 20 <= length (epiteto unHeroe)

--PUNTO SEIS
hacerTarea :: Tarea -> Heroe -> Heroe
hacerTarea unaTarea unHeroe = (agregarTarea unaTarea) . (unaTarea) $ unHeroe

agregarTarea :: Tarea -> Heroe -> Heroe
agregarTarea unaTarea unHeroe = unHeroe {tareas= unaTarea : tareas unHeroe}

--PUNTO SIETE
medirselaHeroes :: Heroe -> Heroe -> (Heroe,Heroe)
medirselaHeroes heroeUno heroeDos
    | ganadorHeroe heroeUno heroeDos= (heroeUno, heroeDos)
    | ganadorHeroe heroeDos heroeUno = (heroeDos, heroeUno)
    | otherwise = medirselaHeroes (hacerLabor heroeUno (tareas heroeDos)) (hacerLabor heroeDos (tareas heroeUno))

ganadorHeroe :: Heroe -> Heroe -> Bool
ganadorHeroe heroeUno heroeDos =
    reconocimiento heroeUno > reconocimiento heroeDos ||
    (igualReconocimiento && sumaRareza heroeUno > sumaRareza heroeDos)
    where igualReconocimiento = reconocimiento heroeUno == reconocimiento heroeDos

sumaRareza :: Heroe -> Int
sumaRareza unHeroe = sum (map rareza (artefactos unHeroe))

--PUNTO OCHO
--se entra en un bucle donde se van a comparar infinitamente

--PUNTO NUEVE, hacer una labor
type Labor = [Tarea]

hacerLabor :: Heroe -> Labor -> Heroe
hacerLabor unHeroe tareas = foldr (hacerTarea) unHeroe tareas

--PUNTO DIEZ
--no, las listas infinitas en foldr se recorreran infinitamente salvo que arrojen el valor o condicion que el programa necesite (Lazy evauation)