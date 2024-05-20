--LISTAS
--1)  Definir una función que sume una lista de números. 
sumaLista :: [Int] -> Int
sumaLista lista = sum lista 


--2) 
frecuencias :: [Int]
frecuencias = [80, 100, 120, 128, 130, 123, 125] 

--a) Definir la función promedioFrecuenciaCardiaca
promLista :: [Int] -> Int
promLista lista = sum lista `div` length lista

promedioFrecuenciaCardiaca :: Int
promedioFrecuenciaCardiaca = promLista frecuencias

--b) Definir la función frecuenciaCardiacaMinuto/1
frecuenciaCardiacaMinuto :: Int -> Int
frecuenciaCardiacaMinuto minuto = frecuencias !! (div (minuto))10

--c) Definir la función frecuenciasHastaMomento
frecuenciasHastaMomento :: Int -> [Int]
frecuenciasHastaMomento minuto = take (div (minuto)10) frecuencias 


--3) esCapicua/1, si data una lista de listas, me devuelve si la concatenación de las sublistas es una lista capicua..
esCapicua :: [String] -> Bool
esCapicua lista = concat lista == reverse (concat lista) --si la lista concatenada es igual a la lista concatenada invertida, entonces es capicua


--4) 
duracionLlamadas :: [(String, [Int])]
duracionLlamadas = [("horarioReducido",[20,10,25,15]),("horarioNormal",[10,5,8,2,9,10])]

--a) Definir la función cuandoHabloMasMinutos, devuelve en que horario se habló más cantidad de minutos
cuandoHabloMasMinutos :: [(String, [Int])] -> String
cuandoHabloMasMinutos llamadas 
    |sum(snd (llamadas !! 0)) > sum(snd (llamadas !! 1)) = fst (llamadas !! 0)
    |sum(snd (llamadas !! 0)) < sum(snd (llamadas !! 1)) = fst (llamadas !! 1)
    |otherwise = "Iguales"

--b) Definir la función cuandoHizoMasLlamadas, devuelve en que franja horaria realizó más cantidad de llamadas
cuandoHizoMasLlamadas :: [(String, [Int])] -> String
cuandoHizoMasLlamadas llamadas
    |length(snd (llamadas !! 0)) > length(snd (llamadas !! 1)) = fst (llamadas !! 0)
    |length(snd (llamadas !! 0)) < length(snd (llamadas !! 1)) = fst (llamadas !! 1)
    |otherwise = "Iguales"

--ORDEN SUPERIOR
--1) Definir la función existsAny/2, que dadas una función booleana y una tupla de tres elementos devuelve True si existe algún elemento de la tupla que haga verdadera la función. 
existsAny :: (a -> Bool) -> (a, a, a) -> Bool
existsAny f (x, y, z) = f x || f y || f z

--2) Definir la función mejor/3, que recibe dos funciones y un número, y devuelve el resultado de la función que dé un valor más alto
mejor :: (Int -> Int) -> (Int -> Int) -> Int -> Int
mejor f1 f2 numero
    |f1 numero > f2 numero = f1 numero
    |f1 numero < f2 numero = f2 numero
    |otherwise = f1 numero --son iguales

--3) Definir la función aplicarPar/2, que recibe una función y un par, y devuelve el par que resulta de aplicar la función a los elementos del par.
aplicarPar :: (Int -> b) -> (Int,Int) -> (b,b)
aplicarPar funcion (x,y)
    |x == y = (funcion x, funcion y)
    |otherwise = (funcion x, funcion y)

--Ejercicios Extras
--1) Definir la función esMultiploDeAlguno/2, que recibe un número y una lista y devuelve True si el número es múltiplo de alguno de los números de la lista. 
esMultiploDeAlguno :: Int -> [Int] -> Bool
esMultiploDeAlguno numero lista = any (\x -> mod numero x == 0) lista