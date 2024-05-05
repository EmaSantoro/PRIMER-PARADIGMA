--Sistema de recuento de las sustancias ya existentes y las nuevas creadas

import Text.Show.Functions
import Data.List

--componente
data Sustancia = Sustancia {
    sustancia :: TipoSustancia, --puede ser un elemento o un compuesto
    especie :: Especie
} deriving (Show)

data TipoSustancia = Sencilla Elemento | Compuesta Elemento CantElem Elemento CantElem deriving (Show)

data Especie = Metal | NoMetal | Halogeno | GasNoble deriving (Show, Eq)

data Elemento = Elemento {
    nombre :: String, 
    simboloQuim :: String,
    numAtomico :: Int,
    tipo :: Especie
} deriving (Show)

data CantElem = CantMoleculas{
    cant :: Int
} deriving (Show) 

-- Obtiene el elemento primario de una sustancia
primario :: TipoSustancia -> Elemento
primario (Sencilla elemento) = elemento
primario (Compuesta elemento1 _ _ _) = elemento1

-- Obtiene el elemento secundario de un TipoPokemon
secundario :: TipoSustancia -> Elemento
secundario (Compuesta _ _ elemento2 _) = elemento2

--sustancias
hidrogeno :: Elemento
hidrogeno = Elemento "Hidrogeno" "H" 1 NoMetal

oxigeno :: Elemento
oxigeno = Elemento "Oxigeno" "O" 8 NoMetal

agua :: Sustancia
agua = Sustancia (Compuesta hidrogeno (CantMoleculas 2) oxigeno (CantMoleculas 1)) NoMetal

--FUNCIONES
