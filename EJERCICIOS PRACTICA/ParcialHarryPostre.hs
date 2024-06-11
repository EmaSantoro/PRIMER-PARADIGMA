import Text.Show.Functions()

--PARCIAL HARRY POSTRE

--PARTE UNO POSTRES
data Postre = Postre{
    sabores :: [Sabor],
    peso :: Float,
    temperatura :: Int
} deriving (Show, Eq)

type Sabor = String

type Hechizo = Postre -> Postre
type Modificador = Postre -> Postre

--Modelado de Hechizos
--Primer Hechizo
incendio :: Hechizo
incendio unPostre = (modificarTemperatura 1) . (modificarPeso (porcentajePeso 5 unPostre)) $ unPostre

modificarTemperatura :: Int -> Modificador
modificarTemperatura grados unPostre = unPostre {temperatura = temperatura unPostre + grados}

modificarPeso :: Float -> Modificador
modificarPeso gramos unPostre = unPostre {peso = peso unPostre + gramos}

porcentajePeso :: Float -> Postre -> Float
porcentajePeso porcentaje unPostre =  (100 * (porcentaje / (peso unPostre)))

--Segundo hechizo
immobulus :: Hechizo
immobulus unPostre = modificarTemperatura (-(temperatura unPostre)) unPostre

--Tercer hechizo
wingardium :: Hechizo
wingardium unPostre = modificarPeso (-(porcentajePeso 10 unPostre)).(agregarSabor "concentrado") $ unPostre

agregarSabor :: String -> Modificador
agregarSabor unSabor unPostre = unPostre {sabores = unSabor : (sabores unPostre)}

--Cuarto hechizo
diffindo :: Float -> Hechizo
diffindo porcentaje unPostre = modificarPeso (porcientoPeso) unPostre
    where porcientoPeso = porcentajePeso porcentaje unPostre

--Quinto hechizo
riddikulus :: Sabor -> Hechizo
riddikulus unSabor unPostre = agregarSabor (reverse unSabor) unPostre

--Sexto hechizo
avadaKedavra :: Hechizo
avadaKedavra = perderSabores . immobulus 

perderSabores :: Modificador
perderSabores unPostre = unPostre {sabores = []}

--Saber si un postre esta listo luego de aplicarle un hechizo
loDejaListo :: Hechizo -> Postre -> Bool
loDejaListo unHechizo unPostre = estaListo (unHechizo unPostre)

estaListo :: Postre -> Bool
estaListo unPostre =
    peso unPostre > 0 &&
    length (sabores unPostre) >= 1 &&
    not (estaCongelado unPostre)


estaCongelado :: Postre -> Bool
estaCongelado unPostre = temperatura unPostre > 0

--Dado unos postres, conocer el peso promedio de los listos
pesoPromedioPostres :: [Postre] -> Float
pesoPromedioPostres listaPostres = (sum (map peso postresListos)) / (fromIntegral(length postresListos))
    where postresListos = (filter estaListo listaPostres)

--PARTE DOS MAGOS
data Mago = Mago {
    hechizos :: [Hechizo],
    horrorcruxes :: Int
} deriving (Show)

--Hacer que un mago asista a una clase de defenza contra cocinas oscuras
asistirAClase :: Mago -> Hechizo -> Postre -> Mago
asistirAClase unMago unHechizo unPostre = (evaluarPractica unHechizo unPostre) . (agregarHechizo unHechizo) $ unMago

agregarHechizo :: Hechizo -> Mago -> Mago
agregarHechizo unHechizo unMago = unMago {hechizos = unHechizo : (hechizos unMago)}

evaluarPractica :: Hechizo -> Postre -> Mago -> Mago
evaluarPractica unHechizo unPostre unMago
    |unHechizo unPostre == avadaKedavra unPostre = sumarHorrorcruxes 1 unMago
    |otherwise = unMago

sumarHorrorcruxes :: Int -> Mago -> Mago
sumarHorrorcruxes cantidad unMago = unMago {horrorcruxes= horrorcruxes unMago + cantidad }

--Hacer que obtenga mejor hechizo 
--Dado un postre y un mago obtener su mejor hechizo, que es aquel de sus hechizos que deja al postre con más cantidad de sabores luego de usarlo.
obtenerMejorHechizo :: Mago -> Postre -> Hechizo
obtenerMejorHechizo unMago unPostre = evaluarMejorHechizo (hechizos unMago) unPostre

evaluarMejorHechizo :: [Hechizo] -> Postre -> Hechizo
evaluarMejorHechizo [x] unPostre = x
evaluarMejorHechizo (x:y:xs) unPostre
    |cantidadSabores (x unPostre) > cantidadSabores (y unPostre) = evaluarMejorHechizo (x : xs) unPostre
    |otherwise = evaluarMejorHechizo (y : xs) unPostre

cantidadSabores :: Postre -> Int
cantidadSabores unPostre = length (sabores unPostre)

--PARTE TRES, INFINITA MAGIA
pastaRola, bizcochueco, deBruine :: Postre
pastaRola = Postre ["Vainilla"] 10 5
bizcochueco = Postre ["Azucar","edulcorante"] 21 8
deBruine = Postre ["Chocolate"] 50 5

type Postres = [Postre]

listaGula :: Postres
listaGula = [pastaRola,deBruine] ++ cycle [bizcochueco]

magoInfinito :: Mago 
magoInfinito = Mago ([diffindo 5] ++ cycle [avadaKedavra]) 2

--Si la condicion es que al menos un postre quede listo, por Lazy Evaluation, haskell al encontrar almenos un True nos va a arrojar un resultado
--A su vez, si la condicion es que todos esten listos, si encuntra un false ya arroja resultado
--En cambio, si nunca encuentra ningun resultado para arrojar, va a recorrer infinitamente la lista de postres

--no existe porque siempre va a comparar los hechizos, la funcion usa recursividad y hasta no verificar todos los de la lista no va a parar.