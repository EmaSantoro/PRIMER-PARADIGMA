--APLICACION PARCIAL
--1) Definir una función siguiente, que al invocarla con un número cualquiera me devuelve el resultado de sumar a ese número el 1. 
siguiente :: Int -> Int
siguiente num = (+1) num

--2) Definir la función mitad que al invocarla con un número cualquiera me devuelve la mitad de dicho número
mitad :: Float -> Float
mitad num = (/ 2) num

--3) Definir una función inversa, que invocando a la función con un número cualquiera me devuelva su inversa
inversa :: Float -> Float
inversa num = (/ num) 1

--4) Definir una función triple, que invocando a la función con un número cualquiera me devuelva el triple del mismo.
triple :: Int -> Int
triple num = (*3) num

--5)Definir una función esNumeroPositivo, que invocando a la función con un número cualquiera me devuelva true si el número es positivo y false en caso contrario. 
esNumeroPositivo :: Int -> Bool
esNumeroPositivo num = (>0) num

--COMPOSICION
--6) Resolver la función esMultiploDe/2, utilizando aplicación parcial y composición.
esMultiploDe :: Int -> Int -> Bool
esMultiploDe numero1 numero2 = (==0) ((mod numero1) numero2)

--7) Resolver la función esBisiesto/1, utilizando aplicación parcial y composición.
esBisiesto :: Int -> Bool
esBisiesto anio = (||) (esMultiploDe 400 anio) ((&&) (esMultiploDe anio 4) ((/=0) (mod anio 100)))

--10) Definir una función esResultadoPar/2, que invocándola con número n y otro m, devuelve true si el resultado de elevar n a m es par. 
esResultadoPar :: Int -> Int -> Bool
esResultadoPar n m = even ((n^) m)