import Text.Show.Functions ()
import Data.List ()

--Parcial Plagio di Plagio

--PUNTO UNO
data Autor = Autor {
    nombre :: String,
    obras :: [Obra]
} deriving (Show)

type Obra = (String, Int)
contenido = fst 
anio = snd 

obraA :: Obra
obraA = ("Había una vez un pato.", 1997)

obraB :: Obra
obraB = ("¡Habia una vez un pato!", 1996)

obraC :: Obra
obraC = ("Mirtha, Susana y Moria.", 2010)

obraD :: Obra
obraD = ("La semántica funcional del amoblamiento vertebral es riboficiente", 2020)

obraE :: Obra
obraE = ("La semantica funcional de Mirtha, Susana y Moria.", 2022)

autor1 = Autor "Ema" [obraA]
autor2 = Autor "Santoro" [obraD, obraE]
autor3 = Autor "EmaSantoro" [obraB, obraC]

--PUNTO DOS
versionCruda :: Obra -> String
versionCruda unaObra = (filter (noEsCaracterEspecial)) . (map cambiarVocal ) $ (contenido unaObra)

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

--PUNTO TRES PLAGIOS
type Plagio = Obra -> Obra -> Bool

copiaLiteral :: Plagio
copiaLiteral unaObra otraObra = versionCruda unaObra == versionCruda otraObra

empiezaIgual :: Int -> Plagio
empiezaIgual caracteres unaObra otraObra = 
    take caracteres (versionCruda unaObra) == take caracteres (versionCruda otraObra) &&
    length (contenido unaObra) < length (contenido otraObra)

leAgregaronIntro :: Plagio
leAgregaronIntro unaObra otraObra =  versionCruda (unaObra) == desecharNoRepetido unaObra otraObra

desecharNoRepetido :: Obra -> Obra -> String
desecharNoRepetido unaObra otraObra= drop (elementosExtras) (versionCruda otraObra)
    where elementosExtras = length (versionCruda otraObra) - length (versionCruda unaObra)

--PUNTO CUATRO
data Bot = Bot {
    nombreBot :: String,
    criterio :: [Plagio]
}

botA :: Bot
botA = Bot "Boris" [copiaLiteral]

botB :: Bot
botB = Bot "Marta" [empiezaIgual 10, leAgregaronIntro]

--PUNTO CINCO
deteccionDePlagio :: Plagio -> Obra -> Obra -> Bool
deteccionDePlagio unCriterio unaObra otraObra = anio unaObra < anio otraObra && unCriterio unaObra otraObra

deteccionDePlagioBot :: Bot -> Plagio
deteccionDePlagioBot unBot unaObra otraObra = any  (\f -> deteccionDePlagio f unaObra otraObra)  (criterio unBot)

--PUNTO SEIS
esCadenaDePlagiadores :: [Autor] -> Bot -> Bool
esCadenaDePlagiadores [] unBot = False
esCadenaDePlagiadores [unAutor] unBot = False
esCadenaDePlagiadores [unAutor, otroAutor] unBot = esPlagiador unBot unAutor otroAutor 
esCadenaDePlagiadores (x:y:xs) unBot = esPlagiador unBot x y && esCadenaDePlagiadores (y:xs) unBot

esPlagiador :: Bot -> Autor -> Autor ->  Bool
esPlagiador unBot unAutor otroAutor = any (esPlagiadorDelAutor unBot unAutor) (obras otroAutor)

esPlagiadorDelAutor :: Bot -> Autor -> Obra ->  Bool
esPlagiadorDelAutor unBot unAutor obra = any (deteccionDePlagioBot unBot obra) (obras unAutor)

--PUNTO SIETE

--PUNTO OCHO

obraInfinita :: Obra
obraInfinita = ("La mona " ++ (cycle "Lisa"), 2022)

--Copia literal: Cuando la lista infinita entra a la funcion versionCruda, se queda en un loop infinito, ya que la funcion filter no puede terminar de recorrer la lista infinita.
--Empieza igual: En estre caso take toma los primero n elementos de la obra infinita por lo que no entra en loop infinito.
--Le agregaron intro: Idem copia literal.