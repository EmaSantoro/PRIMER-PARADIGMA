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