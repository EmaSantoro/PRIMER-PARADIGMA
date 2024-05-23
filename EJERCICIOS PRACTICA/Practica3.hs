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
esMultiploDeAlguno numero lista = any (tieneResto0 numero) lista   --any recorre la lista y devuelve True si alguno cumple la condición

tieneResto0 :: Int -> Int -> Bool
tieneResto0 numero x = mod numero x == 0

--2) Armar una función promedios/1, que dada una lista de listas me devuelve la lista de los promedios de cada lista-elemento.
promedios :: [[Int]] -> [Int]   --recibe una lista de listas y devuelve una lista de enteros
promedios lista = map calculoPromedio lista --Map aplica calculo promedio a todos los elementos de la lista

calculoPromedio :: [Int] -> Int --recibe una lista de enteros y devuelve un entero
calculoPromedio lista = sum lista `div` length lista

--3) Armar una función promediosSinAplazos que dada una lista de listas me devuelve la lista de los promedios de cada lista-elemento, excluyendo menores a 4 
promediosSinAplazos :: [[Int]] -> [Int]
promediosSinAplazos lista = filter (>=4) (map calculoPromedio lista)

--4) Definir la función mejoresNotas, que dada la información de un curso devuelve la lista con la mejor nota de cada alumno
mejoresNotas :: [[Int]] -> [Int]
mejoresNotas lista = map maximum lista

--5) Definir la función aprobó/1, que dada la lista de las notas de un alumno devuelve True si el alumno aprobó. Se dice que un alumno aprobó si todas sus notas son 6 o más. 
aprobo :: [Int] -> Bool
aprobo lista = all (>=6) lista 

--6) Definir la función aprobaron/1, que dada la información de un curso devuelve la información de los alumnos que aprobaron
aprobaron :: [[Int]] -> [[Int]]
aprobaron lista = filter aprobo lista   --Filter filtra los elementos que cumplen la condición de la lista, y me da una lista con los que cumplen

--7) Definir la función divisores/1, que recibe un número y devuelve la lista de divisores
divisores :: Int -> [Int]
divisores numero = filter (esDivisor numero) [1..numero] --filter recorre la lista de 1 a numero y me devuelve los que cumplen la condición

esDivisor :: Int -> Int -> Bool
esDivisor numero1 numero2 = (==0) (mod numero1 numero2)

--8) Definir la función exists/2, que dadas una función booleana y una lista devuelve True si la función da True para algún elemento de la lista.
exist :: (a -> Bool) -> [a] -> Bool
exist funcion lista = any funcion lista

--9)Definir la función hayAlgunNegativo/2, que dada una lista de números y un (…algo…) devuelve True si hay algún nro. negativo.
hayAlgunNegativo :: [Int] -> a ->  Bool
hayAlgunNegativo lista algo = any (<0) lista

--10) Definir la función aplicarFunciones/2, que dadas una lista de funciones y un valor cualquiera, devuelve la lista del resultado de aplicar las funciones al valor
aplicarFunciones :: [a -> b] -> a -> [b]
aplicarFunciones funciones valor = map (\a -> a valor) funciones
--Si se mezclan funciones que dan resultado bool e Int habra error, ya que una lista debe tener elementos del mismo tipo

--11) Definir la función sumaF/2, que dadas una lista de funciones y un número, devuelve la suma del resultado de aplicar las funciones al número.
sumaF :: [Int -> Int] -> Int -> Int
sumaF funciones valor = sum (map (\a -> a valor) funciones)

--12) Escribir una función subirHabilidad/2 que reciba un número (que se supone positivo sin validar) y una lista de números, y le suba la habilidad a cada jugador cuidando que ninguno se pase de 12
subirHabilidad :: Int -> [Int] -> [Int]
subirHabilidad numero lista = map (subirHabilidadJugador numero) lista

subirHabilidadJugador :: Int -> Int -> Int
subirHabilidadJugador numero lista
    |lista + numero > 12 = 12
    |otherwise = lista + numero

--13) Flimitada que recibe una función f y un número n, y devuelve f n garantizando que quede entre 0 y 12 (si f n < 0 debe devolver 0, si f n > 12 debe devolver 12)
fLimitada :: (Int -> Int) -> Int -> Int
fLimitada funcion numero
    |funcion numero < 0 = 0
    |funcion numero > 12 = 12
    |otherwise = funcion numero

--13 a) cambiarHabilidad/2, que reciba una función f y una lista de habilidades, y devuelva el resultado de aplicar f con las garantías de rango que da flimitada
cambiarHabilidad :: (Int -> Int) -> [Int] -> [Int]
cambiarHabilidad funcion habilidades = map (fLimitada funcion) habilidades

--13 b) Usar cambiarCuatro/2 para llevar a 4 a los que tenían menos de 4, dejando como estaban al resto.
cambiarCuatro :: (Int -> Int) -> [Int] -> [Int]
cambiarCuatro funcion habilidades = map (fLimitada' funcion) habilidades

fLimitada' :: (Int -> Int) -> Int -> Int
fLimitada' funcion numero
    |funcion numero < 4 = 4
    |funcion numero > 12 = 12
    |otherwise = funcion numero

--14) Investigar lo que hace la función takeWhile/2
-- takeWhile :: (a -> Bool) -> [a] -> [a] 
-- takeWhile, applied to a predicate pand a listxs, returns the -- longest prefix (possibly empty) of xsof elements that satisfyp`.
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] -- [1,2] 
-- takeWhile (< 9) [1,2,3] -- [1,2,3] 
-- takeWhile (< 0) [1,2,3] -- []

--15) Usar takeWhile/2 para definir las siguientes funciones
--a) primerosPares/1, que recibe una lista de números y devuelve la sublista hasta el primer no par exclusive
primerosPares :: [Int] -> [Int]
primerosPares lista = takeWhile even lista

--b) primerosDivisores/2, que recibe una lista de números y un número n, y devuelve la sublista hasta el primer número que no es divisor de n exclusive
primerosDivisores :: Int -> [Int] -> [Int]
primerosDivisores numero lista = takeWhile (esDivisor numero) lista

--c) primerosNoDivisores/2, que recibe una lista de números y un número n, y devuelve la sublista hasta el primer número que sí es divisor de n exclusive. 
primerosNoDivisores :: Int -> [Int] -> [Int]
primerosNoDivisores numero lista = takeWhile (not.esDivisor numero) lista

--16) 