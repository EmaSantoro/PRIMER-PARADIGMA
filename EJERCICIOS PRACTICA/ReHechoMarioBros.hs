import Text.Show.Functions()

data Plomero = Plomero {
    nombre :: String,
    cajaDeHerramientas :: [Herramienta],
    reparaciones :: [Reparacion],
    dinero :: Float
}deriving (Show)

data Herramienta = Herramienta {
    denominacion :: String,
    precio :: Float,
    material :: Material
}deriving (Show, Eq)

data Material = Hierro | Madera | Goma | Plastico deriving (Show, Eq)

--PUNTO UNO
mario, warrio :: Plomero
mario = Plomero "Mario" [llaveInglesa, martillo] [] 1200
warrio = Plomero "Wario" (iterate (aumentarPrecio 1) llaveFrancesa) [] 0.5

llaveFrancesa, llaveInglesa, martillo :: Herramienta
llaveFrancesa = Herramienta "Llave Francesa" 1 Hierro
llaveInglesa = Herramienta "Llave Inglesa" 200 Hierro
martillo = Herramienta "Martillo" 20 Madera

aumentarPrecio :: Float -> Herramienta -> Herramienta
aumentarPrecio valor unaHerramienta = unaHerramienta {precio= precio unaHerramienta + valor}

--PUNTO DOS
tieneHerramienta :: Herramienta -> Plomero -> Bool
tieneHerramienta unaHerramienta unPlomero = elem (unaHerramienta) (cajaDeHerramientas unPlomero)

esMalvado :: Plomero -> Bool
esMalvado unPlomero = "Wa" == take 2 (nombre unPlomero)

puedeComprarHerramienta :: Herramienta -> Plomero -> Bool
puedeComprarHerramienta unaHerramienta unPlomero = (dinero unPlomero) >= (precio unaHerramienta)

--PUNTO TRES
esBuena :: Herramienta -> Bool
esBuena unaHerramienta = 
    precio unaHerramienta >= 1000 ||
    denominacion unaHerramienta == "martillo" &&
    (esDeMaterial Madera unaHerramienta || esDeMaterial Goma unaHerramienta)

esDeMaterial :: Material -> Herramienta -> Bool
esDeMaterial unMaterial unaHerramienta = material unaHerramienta == unMaterial

--PUNTO CUATRO
comprarHerramienta :: Herramienta -> Plomero -> Plomero
comprarHerramienta unaHerramienta unPlomero
    |puedeComprarHerramienta unaHerramienta unPlomero = (agregarHerramienta unaHerramienta) . (modificarDinero (-(precio unaHerramienta))) $ unPlomero
    |otherwise = unPlomero

modificarDinero :: Float -> Plomero -> Plomero
modificarDinero valor unPlomero = unPlomero {dinero = dinero unPlomero + valor}

agregarHerramienta :: Herramienta -> Plomero -> Plomero
agregarHerramienta unaHerramienta unPlomero = unPlomero {cajaDeHerramientas= unaHerramienta : cajaDeHerramientas unPlomero}

--PUNTO CINCO
data Reparacion = Reparacion{
    descripcion :: String,
    requerimiento :: Condicion
    } deriving (Show)

type Condicion = Plomero -> Bool

filtracionAgua :: Reparacion
filtracionAgua = Reparacion "Filtra el Agua" (tieneHerramienta llaveInglesa)

esReparacionDificil :: Reparacion -> Bool
esReparacionDificil unaReparacion = descripcionComplicada && esUnGrito unaReparacion
    where descripcionComplicada = 100 < longitudDescripcion unaReparacion

esUnGrito :: Reparacion -> Bool
esUnGrito unaReparacion = all (`elem` ['A'..'Z'] ) (descripcion unaReparacion)

presupuestoReparacion :: Reparacion -> Float
presupuestoReparacion unaReparacion = fromIntegral (longitudDescripcion unaReparacion * 3)

longitudDescripcion :: Reparacion -> Int
longitudDescripcion unaReparacion = length (descripcion unaReparacion)

--PUNTO SEIS
hacerReparacion ::  Reparacion -> Plomero -> Plomero
hacerReparacion unaReparacion unPlomero 
    |puedeReparar unaReparacion unPlomero = (evaluarMaligno unaReparacion) . (agregarReparacion unaReparacion ) . (modificarDinero presupuesto) $ unPlomero
    |otherwise = modificarDinero 100 unPlomero
    where presupuesto = (presupuestoReparacion unaReparacion)

puedeReparar :: Reparacion -> Plomero -> Bool
puedeReparar unaReparacion unPlomero =  
    (requerimiento unaReparacion) unPlomero ||
    (esMalvado unPlomero && tieneHerramienta martillo unPlomero)

agregarReparacion :: Reparacion -> Plomero -> Plomero
agregarReparacion unaReparacion unPlomero = unPlomero {reparaciones = unaReparacion : reparaciones unPlomero}

evaluarMaligno :: Reparacion -> Plomero -> Plomero
evaluarMaligno unaReparacion unPlomero
    |esMalvado unPlomero = agregarHerramienta destornillador unPlomero
    |not (esMalvado unPlomero) && esReparacionDificil unaReparacion = perderBuenas unPlomero
    |otherwise = perderUnaHerramienta unPlomero

destornillador :: Herramienta
destornillador = Herramienta "Destornillador" 0 Plastico

perderBuenas :: Plomero -> Plomero
perderBuenas unPlomero = unPlomero {cajaDeHerramientas = filter (not . esBuena) (cajaDeHerramientas unPlomero)}

perderUnaHerramienta :: Plomero -> Plomero
perderUnaHerramienta unPlomero = unPlomero {cajaDeHerramientas = drop 1 (cajaDeHerramientas unPlomero)}

--PUNTO SIETE
pasoDeJornada :: Plomero -> [Reparacion] -> Plomero
pasoDeJornada unPlomero unasReparaciones = foldl (flip hacerReparacion) unPlomero unasReparaciones

--PUNTO OCHO
pasoJornadaEmpleados :: [Plomero] -> [Reparacion] -> [Plomero]
pasoJornadaEmpleados unosPlomeros unasReparaciones = map (flip pasoDeJornada unasReparaciones) unosPlomeros

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

--condicion mas adinerado 
masAdinerado :: [Plomero] -> [Reparacion] -> Plomero
masAdinerado unosPlomeros unasReparaciones = compararPlomeros (condicionMasAdinerado) (pasoJornadaEmpleados unosPlomeros unasReparaciones)

condicionMasAdinerado :: Plomero -> Float
condicionMasAdinerado unPlomero = dinero unPlomero

--condicion mas Inversor
masInversor :: [Plomero] -> [Reparacion] -> Plomero
masInversor unosPlomeros unasReparaciones = compararPlomeros condicionMasInversor (pasoJornadaEmpleados unosPlomeros unasReparaciones)

condicionMasInversor :: Plomero -> Float
condicionMasInversor unPlomero =  sum (map precio (cajaDeHerramientas unPlomero))
