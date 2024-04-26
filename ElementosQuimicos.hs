{-Programa para para llevar un mejor recuento de las sustancias 
ya existentes y las nuevas creadas-}

import Text.Show.Functions
import Data.List


data Especie = Metal | NoMetal | Halogeno | GasNoble deriving (Show, eq)

data Elemento = Elemento {
    nombre :: String, 
    simboloQuim :: String,
    numAtomico :: Int,
    tipo :: Especie
} deriving (Show)

data Compuestos = Compuestos {
    nombreC :: String, 
    simboloQuimC :: String,
    tipoC :: Especie
} deriving (Show)

data TipoSustancia = Sencilla Elemento | Compuesta Compuestos (Show, Eq)

data Sustancia = Sustancia {
    tipoS :: TipoSustancia
} deriving (Show)

--prueba commit

--a las sencillas les corresponde