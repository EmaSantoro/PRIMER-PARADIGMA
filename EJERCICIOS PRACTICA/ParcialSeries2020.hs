import Text.Show.Functions ()
import Data.List ()

--PARCIAL SERIE 2020

data Serie = Serie{
    nombreSerie :: String,
    actores :: [Actor],
    presupuesto :: Float,
    temporadas :: Int,
    rating :: Float,
    cancelada :: Bool
} deriving (Show)

data Actor = Actor{
    nombreActor :: String,
    sueldoAnual :: Float,
    restricciones :: [Restriccion]
} deriving (Show)

type Restriccion= String

--EJEMPLOS
ejemploA = Serie "Stranger Things" ([johnnyDepp, helenaBonham, john]) 100000000 3 8.9 False
john = Actor "John" 20000 []

--PUNTO UNO
--a) saber si la serie esta en rojo
estaEnRojo :: Serie -> Bool
estaEnRojo unaSerie = presupuesto unaSerie < (sum . map sueldoAnual . actores) unaSerie

--b) Saber si una serie es problemática, esto ocurre si tienen más de 3 actores con más de 1 restricción
esProblematica :: Serie -> Bool
esProblematica unaSerie = 3 < (length . filter ((>1) . cantidadRestricciones) . actores) unaSerie
    where cantidadRestricciones = length . restricciones

--PUNTO DOS
type Productor = Serie -> Serie

--a) Con favoritismo
conFavoritismo :: Actor -> Actor -> Productor
conFavoritismo actorFavUno actorFavDos unaSerie = agregarActores . quitarActores 2 $ unaSerie
    where agregarActores = agregarActor actorFavDos . agregarActor actorFavUno

quitarActores :: Int -> Productor
quitarActores cantidad unaSerie = unaSerie {actores= drop cantidad (actores unaSerie)}

agregarActor :: Actor -> Productor
agregarActor actor unaSerie = unaSerie {actores= actor : actores unaSerie}

--b) Tim Burton, siempre agrega a Johnny Depp y helena bonham
johnnyDepp, helenaBonham :: Actor
johnnyDepp = Actor "Johnny Depp" 20000000 []
helenaBonham = Actor "Helena Bonham" 15000000 []

timBurton :: Productor
timBurton = conFavoritismo johnnyDepp helenaBonham

--c) no cambia nada de la serie
gatoPardeitor :: Productor
gatoPardeitor = id

--d) duplica serie
estireitor :: Productor
estireitor unaSerie = unaSerie {temporadas= temporadas unaSerie * 2}

--e) combo de las anteriores
desespereitor :: Productor
desespereitor = estireitor . agregarActor helenaBonham 

--f) : si la serie está en rojo o el rating baja de una cierta cifra, la serie se cancela.
canceleitor :: Float -> Productor
canceleitor cifraRating unaSerie
    | estaEnRojo unaSerie || rating unaSerie < cifraRating = unaSerie {cancelada= True}
    | otherwise = unaSerie  {cancelada= False}

--PUNTO TRES
--Bienestar de una serie

calculoBienestar :: Serie -> Int
calculoBienestar unaSerie
    |cancelada unaSerie = 0
    |otherwise = bienestarPorTemporadas unaSerie + bienestarPorActores unaSerie

bienestarPorTemporadas :: Serie -> Int
bienestarPorTemporadas unaSerie
    |temporadas unaSerie > 4 = 5
    |otherwise = (10 - (temporadas unaSerie)) * 2

bienestarPorActores :: Serie -> Int
bienestarPorActores unaSerie 
    |length (actores unaSerie) < 10 = 3
    |otherwise = 10 - max 2 ((length . filter ((>0) . cantidadRestricciones) . actores) unaSerie)
    where cantidadRestricciones = length . restricciones

--PUNTO CUATRO
type Productores = [Productor]
type Series = [Serie]

productorMasEfectivo :: Series -> Productores -> Series
productorMasEfectivo series productores = map (masEfectivo productores) series

masEfectivo :: Productores -> Serie -> Serie
masEfectivo (x:[]) unaSerie = x unaSerie 
masEfectivo (x:xs) unaSerie
    | calculoBienestar (x unaSerie) > calculoBienestar (head xs $ unaSerie) = x unaSerie
    | otherwise = masEfectivo xs unaSerie

{-PUNTO CINCO
a) Se puede aplicar gatopardeitor en lista infinita de actores?
    la funcion gatoPardeitor devuelve la serie sin modificar, pero si la lista de actores es infinita, mostrara infinitamente los actores de la serie
b) Y a uno con favoritismo?
    Si, se puede aplicar a una lista infinita de actores, ya que solo se agregan dos actores a la serie, y no se recorre la lista de actores infinitamente
-}

--PUNTO SEIS
esControvertida :: Serie -> Bool 
esControvertida unaSerie = comparadorActores (actores unaSerie)

comparadorActores :: [Actor] -> Bool
comparadorActores [] = True
comparadorActores [_] = True
comparadorActores (x:xs)= 
    sueldoAnual x > sueldoAnual (head xs) &&
    comparadorActores xs

--comparadorActores (x : y : xs) = sueldoAnual x > sueldoAnual y && comparadorActores (y : xs)

--PUNTO SIETE
--Explicar la inferencia del tipo de la siguiente función:
--funcionLoca x y = filter (even.x) . map 


-- primero sabemos que hay dos parametro : x e y
-- como la primer funcion que se va a aplicar es map, sabemos que hay un tercer parametro implicito: z
-- z es una lista, no sabemos de que
-- funcionLoca :: -> -> [a] -> 
-- como y recibe la lista de z, debe tener su mismo tipo, pero puede devolver algo de otro tipo. lo unico que 
-- sabemos de este algo es que debe ser una lista, pues luego se le aplica la funcion length
-- funcionLoca :: -> (a -> [b]) -> [a] -> 
-- luego, se aplica filter. sabemos que el map devuelve una lista de Int y que sobre esa lista se aplicara el filter.
-- por lo que x es una funcion que recibe Int y devuelve un Int (ya que luego se le aplica even)
-- finalmente la funcion funcionLoca devuelve una lista de Int:
-- funcionLoca :: (Int -> Int) -> (a -> [b]) -> [a] -> [Int]