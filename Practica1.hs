-- EJERCICIO 1 --
-- Definir la función esMultiploDeTres/1, que devuelve True si un número es múltiplo de 3 --

esMultiploDeTres :: Int -> Bool
esMultiploDeTres numero = mod numero 3 == 0

-- EJERCICIO 2 -- 
-- Definir la función esMultiploDe/2, que devuelve True si el segundo es múltiplo del primero, p.ej.  --

esMultiploDe :: Int -> Int -> Bool
esMultiploDe numero1 numero2 = mod numero1 numero2 == 0

-- EJERCICIO 3 --
-- Definir la función cubo/1, devuelve el cubo de un número --

cubo :: Int -> Int
cubo numero = numero * numero

-- EJERCICIO 4 --
-- Definir la función area/2, devuelve el área de un rectángulo a partir de su base y su altura. --

area :: Num a => a -> a -> a
area base altura = base * altura

--EJERCICIO 5--
--Definir la función esBisiesto/1, indica si un año es bisiesto.--
esBisiesto :: Int -> Bool
esBisiesto anio = esMultiploDe 400 anio || (esMultiploDe anio 4 && mod anio 100 /= 0)

--EJERCICIO 6--
--Definir la función celsiusToFahr/1, pasa una temperatura en grados Celsius a grados Fahrenheit.--
celsiusToFahr :: Float -> Float
celsiusToFahr grados = grados * ( 9 / 5 ) + 32


--EJERCICIO 10--
--Trabajamos con tres números que imaginamos como el nivel del río Paraná a la altura de Corrientes medido en tres días consecutivos--

--Punto A, dispersion,  toma los tres valores y devuelve la diferencia entre el más alto y el más bajo--
dispersion :: Int -> Int -> Int -> Int
minimo :: Int -> Int -> Int -> Int
maximo :: Int -> Int -> Int -> Int
maximo x y z = max (max x y) z
minimo x y z = min (min x y) z
dispersion x y z = maximo x y z - minimo x y z



--Punto B, diasParejos, diasLocos y diasNormales reciben los valores de los tres días. Se dice que son días parejos si la dispersión es chica, --
--que son días locos si la dispersión es grande, y que son días normales si no son ni parejos ni locos.--

diasParejos :: Int -> Int -> Int -> Bool
diasLocos :: Int -> Int -> Int -> Bool
diasNormales :: Int -> Int -> Int -> Bool

diasParejos x y z = dispersion x y z < 30
diasLocos x y z = dispersion x y z > 100
diasNormales x y z = not (diasParejos x y z || diasLocos x y z)

