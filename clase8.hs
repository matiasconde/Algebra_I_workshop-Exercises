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

variaciones :: Eq a => Set a -> Integer -> Set [a] 
variaciones _ 0 = [[]] 
variaciones (a:as) n = agregarTodosATodosLista (a:as) (variaciones (a:as) (n-1))

agregarTodosATodosLista :: Eq a => Set a -> Set [a] -> Set [a]
agregarTodosATodosLista [] b = []
agregarTodosATodosLista (a:as) b = (agregarATodosLista a b)++(agregarTodosATodosLista as b)

agregarATodosLista :: Eq a => a -> Set [a] -> Set [a]
agregarATodosLista a [] = []
agregarATodosLista a (b:bs) = (a:b):(agregarATodosLista a bs)

insertarEn :: Eq a => [a] -> a -> Integer -> [a] --Ojo Length devuelve Int y no matchea con Integer.
insertarEn (a:as) b 1 = [b]++(a:as)
insertarEn (a:as) b n | (toInteger (length (a:as))) + 1 == n = (a:as)++[b]
					  | otherwise = [a]++(insertarEn as b (n-1)) 

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

--bolitasNumeradasEnkCajas :: [Integer] -> Integer -> [[[Integer]]]
--bolitasNumeradasEnCajas n:ns k = (formas1bolitaEnCajas k) (bolitasNumeradasEnCajas ns (k-1)) 

--formasNbolitasUnaCaja n = 

--subconjutos :: Integer -> Integer -> Set (Set Integer)
--subconjutos n 0 = vacio
--subconjuntos n k = 

--que necesito, necesito crear una lista, con listas de cajas llenas de pelotas, (las cajas son listas)
