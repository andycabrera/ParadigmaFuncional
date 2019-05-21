data Animal = UnAnimal {
  coeficienteIntelectual :: Int,
  especie :: String,
  capacidades :: [String] 
} deriving (Show)

bugsBunny = UnAnimal{
  coeficienteIntelectual = 125,
  especie = "Conejo",
  capacidades = ["Burlarse del pelado", "Maestro del disfraz", "Comer zanahorias"]
}

patoLucas = UnAnimal {
  coeficienteIntelectual = 60,
  especie = "Pato",
  capacidades = ["Tendencia a que todo le salga mal"]
}

cerebro = UnAnimal {
  coeficienteIntelectual = 250,
  especie = "Raton",
  capacidades = ["Gran inventor", "Mucha paciencia a su companiero"]
}

pinky = UnAnimal {
  coeficienteIntelectual = 10,
  especie = "Raton",
  capacidades = ["Optimista", "Inventa palabras graciosas"]
}

dumbo = UnAnimal {
  coeficienteIntelectual = 80,
  especie = "Elefante",
  capacidades = ["Volar usando sus orejas"]
}

------------------------------------------------------------------ Mis estructuras

inteligenciaSuperior :: Animal -> Int -> Animal
inteligenciaSuperior animal n = animal{coeficienteIntelectual = (coeficienteIntelectual animal) + n}

pinkyficar :: Animal -> Animal
pinkyficar animal = animal{capacidades=[]}

superpoderes :: Animal -> Animal
superpoderes animal 
  | especie animal == "Elefante" = animal{capacidades=("No tenerle miedo a los ratones":(capacidades animal))} ---------Hasta acá punto 2
  | especie animal == "Raton" && coeficienteIntelectual animal > 100 = animal{capacidades=("Hablar":(capacidades animal))}
  | otherwise = animal

pinkiesco :: String -> Bool
pinkiesco cadena = length cadena > 10

antropomorfico :: Animal -> Bool
antropomorfico animal = (any (== "Hablar") (capacidades animal)) && (coeficienteIntelectual animal > 60) ----------- Hasta acá punto 3

noTanCuerdo :: Animal -> Bool
noTanCuerdo animal = (length (filter (pinkiesco) (capacidades animal))) > 2