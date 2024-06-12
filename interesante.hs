--FILTRAR O SELECCIONAR POR TIPO DE CARACTERES
    filter noEsCaracterEspecial listaString

noEsCaracterEspecial :: Char -> Bool
noEsCaracterEspecial caracter = elem caracter letrasOnumeros

letrasOnumeros = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ']

cambiarVocal :: Char -> Char
cambiarVocal 'á' = 'a'
cambiarVocal 'é' = 'e'
cambiarVocal 'í' = 'i'
cambiarVocal 'ó' = 'o'
cambiarVocal 'ú' = 'u'
cambiarVocal caracter = caracter

esUnGrito :: Reparacion -> Bool
esUnGrito unaReparacion = all (`elem` ['A'..'Z'] ) (descripcion unaReparacion)

--RECURSIVA ELEGIR GEMA QUE DA MAYOR ENERGIA A UN PERSONAJE
elegirPoderosa :: [Gema] -> Personaje -> Gema
elegirPoderosa [x] _ = x
elegirPoderosa (x:y:xs) personaje 
    |energia (x personaje) < energia (y personaje) = elegirPoderosa (y:xs) personaje
    |otherwise= elegirPoderosa (x:xs) personaje

--APLICAR LISTA DE FUNCIONES A UN ELEMENTO
utilizarGemas :: [Gema] -> Personaje -> Personaje
utilizarGemas gemas oponente= foldr ($) oponente (gemas)

--REPETIR DOS VECES LA APLICACION DE UNA FUNCION
laLoca :: Gema -> Gema
laLoca gema oponente = foldr ($) oponente (take 2 (repeat gema)) 


--REPETIR TRES VECES LA APLICACION DE UNA FUNCION
tripleMovimientoTurbo :: Poder
tripleMovimientoTurbo unaNave = foldr ($) unaNave (take 3 (repeat movimientoTurbo)) 

--COMPARACION DE COMO AFECTAN DOS ESTRATEGIAS A UNA FLOTA
type Estrategia = Nave -> Bool
misionSorpresa :: Estrategia -> Flota -> Nave -> Flota
misionSorpresa estrategia flota naveAtacante = atacarFlota . filter (estrategia) $ flota
    where atacarFlota = map (atacarNave naveAtacante)

compararEstrategias :: Estrategia -> Estrategia -> Flota -> Nave -> Flota
compararEstrategias estrategia1 estrategia2 flota naveAtacante
    |durabilidadFlota (misionEstrategia1) > durabilidadFlota (misionEstrategia2) = misionEstrategia2
    |otherwise = misionEstrategia1
        where   misionEstrategia1 = misionSorpresa estrategia1 flota naveAtacante
                misionEstrategia2 = misionSorpresa estrategia2 flota naveAtacante

--APLICAR LISTA DE FUNCIONES A LISTA DE ELEMENTOS
pasoDeJornada :: Plomero -> [Reparacion] -> Plomero
pasoDeJornada unPlomero unasReparaciones = foldl (flip hacerReparacion) unPlomero unasReparaciones

pasoJornadaEmpleados :: [Plomero] -> [Reparacion] -> [Plomero]
pasoJornadaEmpleados unosPlomeros unasReparaciones = map (flip pasoDeJornada unasReparaciones) unosPlomeros




--COMPARACION CON CONDICION DADA POR UNA FUNCION
type Comparador = Plomero -> Float
compararPlomeros :: Comparador -> [Plomero] -> Plomero
compararPlomeros comparador [x] = x
compararPlomeros comparador (x:y:xs) 
    |comparador x > comparador y = compararPlomeros comparador (x:xs) 
    |otherwise = compararPlomeros comparador (y:xs) 

--condicion mas reparador
masReparador :: [Plomero] -> [Reparacion] -> Plomero
masReparador unosPlomeros unasReparaciones = compararPlomeros condicionMasReparador (pasoJornadaEmpleados unosPlomeros unasReparaciones)

condicionMasReparador :: Plomero -> Float
condicionMasReparador unPlomero = fromIntegral (length (reparaciones unPlomero))