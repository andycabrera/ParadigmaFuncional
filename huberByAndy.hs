import Text.Show.Functions
-- HUBER

-- PUNTO 1
data Chofer = Chofer{
nombreC :: String,
km :: Float,
viajes :: [Viaje],
condicion :: (Viaje -> Bool)
} deriving (Show)

data Viaje = Viaje{
fecha :: (Int, Int, Int),
cliente :: Cliente,
costo :: Int
} deriving (Show)

data Cliente = Cliente{
nombre :: String,
direccion :: String
}deriving (Show)


-- PUNTO 2

cualquierVaje :: Viaje -> Bool
cualquierVaje viaje = True

masDoscientos :: Viaje -> Bool
masDoscientos (Viaje _ _ costo) = (>200) costo

masDeN :: Int -> Viaje ->  Bool
masDeN letras (Viaje _ cliente _)  = (length.nombre)cliente > letras

zonaDet :: String -> Viaje ->  Bool
zonaDet zona (Viaje _ cliente _) = ((esDistinto zona).direccion) cliente   

esDistinto :: String -> String -> Bool
esDistinto zona lugarViaje  = zona /= lugarViaje

-- PUNTO 3

lucas = Cliente{
nombre = "Lucas",
direccion = "Victoria"
}

daniel = Chofer{
nombreC = "Daniel",
km = 23500,
viajes = [danielLucas],
condicion = (zonaDet "Olivos")
}

danielLucas = Viaje{
fecha = (20,04,2017),
cliente = lucas,
costo = 150
}

alejandra = Chofer{
nombreC = "Alejandra",
km = 180000,
viajes = [],
condicion = (cualquierVaje)
}

--PUNTO 4

puedeTomar :: Viaje -> Chofer -> Bool
puedeTomar viaj (Chofer _ _ _ cond) = cond viaj

--PUNTO 5

liquidación :: Chofer -> Int
liquidación (Chofer _ _ viajes _) = (sum.map costo) viajes


--PUNTO 6

realizarViaje :: Viaje -> [Chofer] -> Chofer
realizarViaje viaje choferes = ((efectuarViaje viaje).considerarChof.(filtrarChoferes viaje)) choferes

filtrarChoferes :: Viaje -> [Chofer] -> [Chofer]
filtrarChoferes viaje choferes = filter (puedeTomar viaje) choferes

considerarChof :: [Chofer] -> Chofer
considerarChof choferes = foldl1 (comparaChof) choferes 

comparaChof :: Chofer -> Chofer -> Chofer
comparaChof chof1 chof2 
  |(length.viajes) chof1 > (length.viajes) chof2 = chof1
  | otherwise = chof2 
	
efectuarViaje :: Viaje -> Chofer -> Chofer
efectuarViaje viaje (Chofer nomb km viajes cond) = Chofer nomb km (viajes ++ [viaje]) cond 

--PUNTO 7

(1 punto) Al infinito y más allá
Modelar al chofer “Nito Infy”, su auto tiene 70.000 kms., que el 11/03/2017 hizo infinitos viajes de $ 50 con Lucas y toma cualquier viaje donde el cliente tenga al menos 3 letras. Puede ayudarse con esta función:

repetirViaje viaje = viaje : repetirViaje viaje

¿Puede calcular la liquidación de Nito? Justifique.
¿Y saber si Nito puede tomar un viaje de Lucas de $ 500 el 2/5/2017? Justifique. 

nito = Chofer{
nombreC = "Nito Infy",
km = 70000,
viajes = [],
condicion = (masDeN 3)
}

-- a) No puede calcular la liquidacion ya que la lista de viajes de nito infy es una lista enifinita y al sacar la liquidacion entraria en un siclo loop que nunca terminaria
-- b) Se puede tomar el viaje ya que no necesita usar el atributo viajes de nito infy

-- PUNTO 8

gongNeng :: Ord c => c -> (c -> Bool) -> (a -> c) -> [a] -> c










