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