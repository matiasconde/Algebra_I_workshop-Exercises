multiplo7 :: [Integer] -> [Integer]
multiplo7 [] = []
multiplo7 (x:xs) | mod x 7 == 0 = x:(multiplo7 xs)
			     | otherwise = multiplo7 xs

type Set a = [a]

vacio :: Eq a => Set a
vacio = []

agregar :: Eq a => a -> Set a -> Set a
agregar n s | elem n s = s  -- elem es una función build in haskell
		    | otherwise = (n:s)

--agregarSet :: Eq a => Set a -> Set (Set a) -> Set (Set a)
--agregarSet a b | elem a b 

incluido :: Eq a => Set a -> Set a -> Bool
incluido [] _ = True
incluido (a:as) b = (elem a b)&&(incluido as b)

iguales :: Eq a => Set a -> Set a -> Bool
iguales a b = (incluido a b)&&(incluido b a)

agregarATodos :: Eq a => a -> Set (Set a) -> Set (Set a)
agregarATodos n [] = []
agregarATodos n (a:as) | elem n a = (agregarATodos n as)
				     | otherwise = agregar (agregar n a) (agregarATodos n as) -- En el curso se usó directamente ":" en lugar de agregar.

partes :: Eq a => Set a -> Set (Set a)
partes [] = [[]]
partes (a:as) = (agregarATodos a (partes as)) ++ (partes as)

auxiliarParaProductoCartesiano :: Eq a => a -> Set a -> Set (a,a)
auxiliarParaProductoCartesiano a [] = []
auxiliarParaProductoCartesiano a (b:bs) = agregar (a,b) (auxiliarParaProductoCartesiano a bs)

productoCartesiano :: Eq a => Set a -> Set a -> Set (a,a)
productoCartesiano [] bs = []
productoCartesiano (a:as) bs = (auxiliarParaProductoCartesiano a bs)++(productoCartesiano as bs)

-- Implementación de la función Variaciones, con todas sus auxiliares: 
variaciones :: Eq a => Set a -> Integer -> Set [a] 
variaciones _ 0 = [[]] 
variaciones (a:as) n = agregarTodosATodosLista (a:as) (variaciones (a:as) (n-1))

agregarTodosATodosLista :: Eq a => Set a -> Set [a] -> Set [a]
agregarTodosATodosLista [] b = []
agregarTodosATodosLista (a:as) b = (agregarATodosLista a b)++(agregarTodosATodosLista as b)

agregarATodosLista :: Eq a => a -> Set [a] -> Set [a]
agregarATodosLista a [] = []
agregarATodosLista a (b:bs) = (a:b):(agregarATodosLista a bs)


-- Implementación de la función Permutaciones, con todas sus auxiliares:
permutaciones :: Integer -> [[Integer]]
permutaciones 1 = [[1]]
permutaciones n = insertarEnTodasLasPosicionesLista n (permutaciones (n-1))

insertarEnTodasLasPosicionesLista :: Integer -> [[Integer]] -> [[Integer]]
insertarEnTodasLasPosicionesLista n [] = []
insertarEnTodasLasPosicionesLista n (a:as) = (insertarEnTodasLasPosiciones n a) ++ (insertarEnTodasLasPosicionesLista n as)

insertarEnTodasLasPosiciones :: Integer -> [Integer] -> [[Integer]]
insertarEnTodasLasPosiciones n [] = []
insertarEnTodasLasPosiciones n l = insertarEnTodasLasPosicionesHasta l n ((toInteger (length l))+1)

insertarEnTodasLasPosicionesHasta :: [Integer] -> Integer -> Integer -> [[Integer]]
insertarEnTodasLasPosicionesHasta l n 1 = [(insertarEn l n 1)]
insertarEnTodasLasPosicionesHasta l n k = [(insertarEn l n k)]++(insertarEnTodasLasPosicionesHasta l n (k-1))

insertarEn :: Eq a => [a] -> a -> Integer -> [a] --Ojo Length devuelve Int y no matchea con Integer.
insertarEn (a:as) b 1 = [b]++(a:as)
insertarEn (a:as) b n | (toInteger (length (a:as))) + 1 == n = (a:as)++[b]
					  | otherwise = [a]++(insertarEn as b (n-1)) 



-- Implementación de la función Repartir n bolas numeradas en k cajas posicionadas, con todas sus auxiliares y algunas extrsa de prueba. (única auxiliar fuera de este scope es "agregarATodosLista" que se creó en la implementación de "Variaciones"

generarNcajasVacias :: Integer -> [[Integer]]  --Función extra de prueba
generarNcajasVacias 1 = [[]]
generarNcajasVacias n = []:(generarNcajasVacias (n-1))

agregarAlPrimero :: a -> [[a]] -> [[a]] ----Función extra de prueba
agregarAlPrimero a (x:xs) = [(a:x)]++(xs)

agregarATodosPrimerosElemLista :: Eq a => a -> [[[a]]] -> [[[a]]]  --Función extra de prueba
agregarATodosPrimerosElemLista a [] = []
agregarATodosPrimerosElemLista a (b:bs) = (agregarAlPrimero a b):(agregarATodosPrimerosElemLista a bs)

repartir1BolitaKcajas :: Integer -> [[[Integer]]] --Auxiliar
repartir1BolitaKcajas 1 = [[[1]]]
repartir1BolitaKcajas k = ([1]:(generarNcajasVacias (k-1))):(agregarATodosLista [] (repartir1BolitaKcajas (k-1)))

bolitasNumeradasEnkCajas :: Integer -> Integer -> [[[Integer]]] --Función Principal
bolitasNumeradasEnkCajas 1 k = repartir1BolitaKcajas k
bolitasNumeradasEnkCajas n k = (repartirBolaNenTodasReparticiones n (bolitasNumeradasEnkCajas (n-1) k))

repartirBolaNenUnaReparticion :: Integer -> [[Integer]] -> [[[Integer]]] --Auxiliar
repartirBolaNenUnaReparticion n [] = []
repartirBolaNenUnaReparticion n (x:xs) = [((n:x):xs)]++(agregarATodosLista x (repartirBolaNenUnaReparticion n xs)) 

repartirBolaNenTodasReparticiones :: Integer -> [[[Integer]]] -> [[[Integer]]] --Auxiliar
repartirBolaNenTodasReparticiones n [] = [] 
repartirBolaNenTodasReparticiones n (x:xs) = (repartirBolaNenUnaReparticion n x)++(repartirBolaNenTodasReparticiones n xs)


--Implementación de Seleccionar todas las listas ordenadas de k numeros entre n números, con todas sus funciones auxiliares:

pertenece1 :: Integer -> [Integer] -> Bool --Auxiliar
pertenece1 a [] = False
pertenece1 a (x:xs) = (a == x) || (pertenece1 a xs)

pertenece :: [Integer] -> [[Integer]] -> Bool --Auxiliar
pertenece a [] = False
pertenece a (x:xs) = (a == x) || (pertenece a xs)

hayRepetidos :: [Integer] -> Bool --Auxiliar
hayRepetidos [] = False
hayRepetidos (x:[]) = False
hayRepetidos (x:xs) = (pertenece1 x xs) || (hayRepetidos xs)

minimoLista :: [Integer] -> Integer --Pensada solo con Naturales --Auxiliar
minimoLista [] = 0
minimoLista (x:[]) = x
minimoLista (x:xs) | x < (minimoLista xs) = x
      			   | otherwise = minimoLista xs

eliminarPrimeraAparicion :: [Integer] -> Integer -> [Integer] --Auxiliar
eliminarPrimeraAparicion [] a = []
eliminarPrimeraAparicion (x:xs) a | a==x = xs
								  | otherwise = x:(eliminarPrimeraAparicion xs a)

ordenar :: [Integer] -> [Integer] --Auxiliar
ordenar [] = []
ordenar (x:xs) | x == minimoLista (x:xs) = x:(ordenar xs)
			 | otherwise = (minimoLista xs):(ordenar listaOrdenadaSinMinimo)
		     where listaOrdenadaSinMinimo = ordenar (x:(eliminarPrimeraAparicion xs (minimoLista xs)))

eliminarListConRep :: [[Integer]] -> [[Integer]] --Auxiliar
eliminarListConRep [] = []
eliminarListConRep (x:xs) | hayRepetidos x = eliminarListConRep xs
						  | otherwise = x:(eliminarListConRep xs)

ordTodasLasListas :: [[Integer]] -> [[Integer]] --Auxiliar
ordTodasLasListas [] = []
ordTodasLasListas (x:xs) = (ordenar x):(ordTodasLasListas xs)

elimListasRepetidas :: [[Integer]] -> [[Integer]] --Auxiliar
elimListasRepetidas [] = []
elimListasRepetidas (x:xs) | (pertenece x xs) = elimListasRepetidas xs
						   | otherwise = x:(elimListasRepetidas xs)

hayRepetidosListas :: [[Integer]] -> Bool --Auxiliar
hayRepetidosListas [] = False
hayRepetidosListas (x:[]) = False
hayRepetidosListas (x:xs) = (x == head xs) || (hayRepetidosListas xs)

selecTodasKordEntreN :: Integer -> Integer -> [[Integer]] --Función Principal
selecTodasKordEntreN 0 k = []
selecTodasKordEntreN n k = elimListasRepetidas (ordTodasLasListas (eliminarListConRep (variaciones [1..n] k)))

-- Sucesiones de 3 1's y 3 0's: 

sumaLista :: [Integer] -> Integer --Auxiliar
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

elegirElemSumak :: Integer -> [[Integer]] -> [[Integer]] --Auxiliar
elegirElemSumak k [] = []
elegirElemSumak k (x:xs) | sumaLista x == k = x:(elegirElemSumak k xs)
						 | sumaLista x /= k = (elegirElemSumak k xs)


sucesiones6x01 :: [[Integer]] --Función Principal
sucesiones6x01 = elegirElemSumak 3 (variaciones [0,1] 6)

-- Sucesiones de 5 elementos donde hay más 1's que 0's

elegirElemSumaMasdeK :: Integer -> [[Integer]] -> [[Integer]] --Auxiliar
elegirElemSumaMasdeK k [] = []
elegirElemSumaMasdeK k (x:xs) | (sumaLista x) >= k = x:(elegirElemSumaMasdeK k xs)
            				  | otherwise = (elegirElemSumak k xs)

sucesiones5x01 :: [[Integer]] --Función Principal
sucesiones5x01 = elegirElemSumaMasdeK 3 (variaciones [1,0] 5) --No estaría comprendiendo porque si llamo en [0,1] me da menos elementos.


-- Subconjuntos de k elementos: 

elegirElemLongk :: Integer -> Set (Set (Integer)) -> Set (Set (Integer)) --Auxiliar
elegirElemLongk k [] = []
elegirElemLongk k (x:xs) | toInteger(length x) == k = x:(elegirElemLongk k xs)
				         | otherwise = (elegirElemLongk k xs)

subconjuntos :: Integer -> Integer -> Set (Set (Integer))
subconjuntos n 0 = [[]]
subconjuntos n k = elegirElemLongk k (partes [1..n]) --Función Principal

