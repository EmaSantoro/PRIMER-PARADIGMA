import Text.Show.Functions ()
import Data.List (genericLength)

--PUNTO UNO
data Plomero = Plomero{
    nombre :: String,
    dinero :: Float,
    cajaHerramientas :: [Herramienta],
    reparaciones :: [Reparacion]
}deriving (Show)

data Herramienta = Herramienta{
    nombreHerramienta :: String,
    precio :: Float,
    material :: Material
}deriving (Show, Eq)

data Reparacion = Reparacion {
    descripcion :: String,
    condicion :: Condicion
}deriving (Show)

type Condicion = Plomero -> Bool

data Material = Hierro | Madera | Goma | Plastico deriving (Show, Eq)

--modelar a los plomeros y sus herramientas
mario :: Plomero
mario = Plomero "Mario" 1200 [martillo, llaveInglesa] []

martillo :: Herramienta
martillo = Herramienta "Martillo" 20 Madera

llaveInglesa :: Herramienta
llaveInglesa = Herramienta "Llave Inglesa" 200 Hierro

wario :: Plomero
wario = Plomero "Wario" 0.5 (repetirHerramienta llaveFrancesa) []

repetirHerramienta :: Herramienta -> [Herramienta]
repetirHerramienta herramienta = iterate (aumentarPrecio 1) herramienta

aumentarPrecio :: Float -> Herramienta -> Herramienta
aumentarPrecio cantidad herramienta = herramienta {precio = precio herramienta + cantidad}

llaveFrancesa :: Herramienta
llaveFrancesa = Herramienta "Llave Francesa" 1 Hierro

--PUNTO DOS
tieneHerramienta :: Herramienta -> Plomero -> Bool
tieneHerramienta herramienta = elem herramienta . cajaHerramientas

esMalvado :: Plomero -> Bool
esMalvado unPlomero= "Wa" == take 2 (nombre unPlomero)

puedeComprarHerramienta :: Herramienta -> Plomero -> Bool
puedeComprarHerramienta unaHerramienta unPlomero = (precio unaHerramienta) <= (dinero unPlomero)

--PUNTO TRES
esHerramientaBuena :: Herramienta -> Bool
esHerramientaBuena unaHerramienta =
    precio unaHerramienta > 10000 ||
    esMartillo unaHerramienta && material unaHerramienta == Madera ||
    esMartillo unaHerramienta && material unaHerramienta == Goma

esMartillo :: Herramienta -> Bool
esMartillo unaHerramienta = nombreHerramienta unaHerramienta == "Martillo"

--PUNTO CUATRO
comprarHerramienta :: Herramienta -> Plomero -> Plomero
comprarHerramienta unaHerramienta unPlomero 
    |puedeComprarHerramienta unaHerramienta unPlomero = adquirirHerramienta unaHerramienta unPlomero
    |otherwise = unPlomero

adquirirHerramienta :: Herramienta -> Plomero -> Plomero
adquirirHerramienta unaHerramienta unPlomero = sumarHerramienta unaHerramienta . restarDinero unaHerramienta $ unPlomero

restarDinero :: Herramienta -> Plomero -> Plomero
restarDinero unaHerramienta unPlomero = unPlomero {dinero = dinero unPlomero - precio unaHerramienta}

sumarHerramienta :: Herramienta -> Plomero -> Plomero
sumarHerramienta unaHerramienta unPlomero = unPlomero {cajaHerramientas = unaHerramienta : cajaHerramientas unPlomero}

--PUNTO CINCO
--reparaciones modeladas al inicio
filtracionAgua :: Reparacion
filtracionAgua = Reparacion "Filtracion de Agua" (tieneHerramienta llaveInglesa)

esReparacionDificil :: Reparacion -> Bool
esReparacionDificil unaReparacion = esDescripcionDificil unaReparacion || esUnGrito unaReparacion

esDescripcionDificil :: Reparacion -> Bool
esDescripcionDificil unaReparacion = length (descripcion unaReparacion) > 100

esUnGrito :: Reparacion -> Bool
esUnGrito unaReparacion = all esMayuscula (descripcion unaReparacion)

esMayuscula :: Char -> Bool
esMayuscula unaLetra = elem unaLetra ['A'..'Z']

presupuestoReparacion :: Reparacion -> Float
presupuestoReparacion unaReparacion = fromIntegral (length (descripcion unaReparacion) * 300)

--PUNTO SEIS
reparar :: Reparacion -> Plomero -> Plomero
reparar unaReparacion unPlomero
    |puedeReparar unaReparacion unPlomero = condicionMalvado unaReparacion . hacerReparacion unaReparacion $ unPlomero
    |otherwise = unPlomero {dinero = dinero unPlomero + 100}

hacerReparacion :: Reparacion -> Plomero -> Plomero
hacerReparacion unaReparacion unPlomero = agregarReparacion (sumarDineroReparacion unPlomero (presupuestoReparacion unaReparacion)) $ unaReparacion

puedeReparar :: Reparacion -> Plomero -> Bool
puedeReparar unaReparacion unPlomero = 
    condicion unaReparacion unPlomero || 
    esMalvado unPlomero && tieneHerramienta martillo unPlomero

agregarReparacion :: Plomero -> Reparacion -> Plomero
agregarReparacion unPlomero unaReparacion = unPlomero {reparaciones = unaReparacion : reparaciones unPlomero}

sumarDineroReparacion :: Plomero -> Float -> Plomero
sumarDineroReparacion unPlomero dineroSuma = unPlomero {dinero = dinero unPlomero + dineroSuma}

condicionMalvado ::  Reparacion -> Plomero -> Plomero
condicionMalvado unaReparacion unPlomero
    |esMalvado unPlomero = adquirirHerramienta destornillador unPlomero
    |esReparacionDificil unaReparacion = perderHerramientasBuenas unPlomero
    |otherwise = unPlomero {cajaHerramientas = drop 1 (cajaHerramientas unPlomero)}

destornillador :: Herramienta
destornillador = Herramienta "Destornillador" 0 Plastico

perderHerramientasBuenas :: Plomero -> Plomero
perderHerramientasBuenas unPlomero = unPlomero {cajaHerramientas = filter (not . esHerramientaBuena) (cajaHerramientas unPlomero)}

--PUNTO SIETE
consecuenciasJornada :: [Reparacion] -> Plomero -> Plomero
consecuenciasJornada reparaciones unPlomero = foldl (flip reparar) unPlomero reparaciones

--PUNTO OCHO
type Plomeros = [Plomero]
empleadosPostJornada :: Plomeros -> [Reparacion] -> Plomeros
empleadosPostJornada plomeros reparaciones = map (consecuenciasJornada reparaciones) plomeros

type Condicional = Plomero -> Float

compararEmpleado :: Condicional -> Plomeros -> Plomero
compararEmpleado condicion (plomero1 : plomero2 : plomeros)
    |condicion plomero1 > condicion plomero2 = plomero1
    |condicion plomero1 < condicion plomero2 = plomero2
    |otherwise = compararEmpleado condicion (plomero2 : plomeros)

--a)El empleado más reparador: El plomero que más reparaciones tiene en su historial una vez realizada su jornada laboral.
empleadoMasReparador :: Plomeros -> [Reparacion] -> Plomero
empleadoMasReparador plomeros reparaciones =  compararEmpleado condicionReparaciones (empleadosPostJornada plomeros reparaciones)

condicionReparaciones :: Plomero -> Float
condicionReparaciones unPlomero = fromIntegral (length (reparaciones unPlomero))

--b)El empleado más adinerado: El plomero que más dinero tiene encima una vez realizada su jornada laboral.
empleadoMasAdinerado :: Plomeros -> [Reparacion] -> Plomero
empleadoMasAdinerado plomeros reparaciones = compararEmpleado condicionDinero (empleadosPostJornada plomeros reparaciones)

condicionDinero :: Plomero -> Float
condicionDinero unPlomero = dinero unPlomero

--c)El empleado que más invirtió: El plomero que más plata invertida tiene entre las herramientas que le quedaron una vez realizada su jornada laboral.
empleadoQueMasInvirtio :: Plomeros -> [Reparacion] -> Plomero
empleadoQueMasInvirtio plomeros reparaciones = compararEmpleado condicionInversion (empleadosPostJornada plomeros reparaciones)

condicionInversion :: Plomero -> Float
condicionInversion unPlomero = sum (map precio (cajaHerramientas unPlomero))

--Para probar agrego a luigi
luigi :: Plomero
luigi = Plomero "Luigi" 100 [llaveFrancesa] []

--para probar agrego destapar cañeria
destaparCanieria :: Reparacion
destaparCanieria = Reparacion "Destapar Cañeria" (tieneHerramienta llaveFrancesa)