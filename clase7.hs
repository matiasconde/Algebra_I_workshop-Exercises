--LISTAS

--Las listas son “listas” de elementos de un mismo tipo. Los elementos se pueden repetir.
--Por ejemplo:
--I [1, 2, 1]
--I [True, False, False, True]
--I [] (lista vacı́a)


--ALGUNAS OPERACIONES: 
--I head :: [a] -> a
--I tail :: [a] -> [a] (osea saca el primer elemento y me da la lista cola
--I (:) :: a -> [a] -> [a]
--I (++) :: [a] -> [a] -> [a]
--I length :: [a] -> Integer

-- head [(1,2),(3,4),(5,2)] devuelve (Integer,Integer)
--(1,2)
-- tail [1,2,3,4,4,3,2,1] devuelve [Integer]
--[2,3,4,4,3,2,1]
-- head [] devuelve nada (pues no hay nada)
--exception
-- head [1,2,3]:[2,3] como haskell es lasy, primero evalua a la izquierda luego eso me da el primer elemento 1, y lo mete en la derecha, osea devuelve [Integer]
--[1,2,3]
--[True, True] ++ [False, False] devuelve una [Bool]
--[True, True,False, False]
--[1,2] : [] [[Integer]] pues lo de la derecha es [] y haskell dice, ahí puedo meter cualquier a, este a es [Integer] luego lo meto.
--[[1,2]]


listar :: a->a->a-> [a]
listar x y z = [x,y,z]

listar2 :: a->a->a-> [a]
listar2 x y z = x:y:z:[]

--Escribir una expresión que denote la lista estrictamente --decreciente de enteros que comienza
--con el número 1 y termina con el número -100.

-- [1,0..-100]

-- RECURSION EN LISTAS
sumatoria :: [Integer] -> Integer
sumatoria a | length a == 0 = 0
		    | otherwise = head a + sumatoria (tail a)

pertenece :: Eq a => a->[a] -> Bool
pertenece a b | length b == 0 = False
  			  | otherwise = (a== (head b)) || pertenece a (tail b)

-- PM comes from "Pattern matching"
pertenecePM ::  Eq a => a->[a]->Bool
pertenecePM x [] = False
pertenecePM y (x:xs) = (y == x) || (pertenecePM y xs)

productoria :: [Integer] -> Integer
productoria xs | length xs == 0 = 1
		       | otherwise = (head xs)*(productoria (tail xs))

productoriaPM :: [Integer] -> Integer
productoriaPM [] = 1
productoriaPM (x:xs) = x*(productoria xs)

sumarN :: Integer -> [Integer] -> [Integer]
sumarN n xs | length xs == 0 = []
 		  | otherwise = (n + head xs):(sumarN n (tail xs))

sumarNPM :: Integer -> [Integer] -> [Integer]
sumarNPM n [] = []
sumarNPM n (x:xs) = (n + x):(sumarNPM n xs)

ultimoElemento :: [a]->[a]
ultimoElemento xs | length xs == 0 = []
				  | length xs == 1 = xs 
				  | otherwise = ultimoElemento (tail xs)

sumarElUltimo :: [Integer]->[Integer]
sumarElUltimo xs | length xs == 0 = []
				 | otherwise = sumarN (head (ultimoElemento xs)) xs

sumarElUltimoPM :: [Integer]->[Integer]
sumarElUltimoPM [] = []
sumarElUltimoPM (x:xs) = sumarNPM (head (ultimoElemento (x:xs))) (x:xs)

sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero xs = sumarN (head xs) xs 

sumarElPrimeroPM :: [Integer] -> [Integer]
sumarElPrimeroPM [] = []
sumarElPrimeroPM (x:xs) = sumarNPM x (x:xs)

pares :: [Integer] -> [Integer]
pares xs | length xs == 0 = []
		 | mod (head xs) 2 == 0 = (head xs):(pares (tail xs))
	     | otherwise = pares (tail xs)

--paresPM :: [Integer] -> [Integer]
--paresPM [] = []
--paresPM x:xs = 

multiplosDeN :: Integer->[Integer]->[Integer]
multiplosDeN n xs | length xs == 0 = []
				  | mod (head xs) n == 0 = (head xs):(multiplosDeN n (tail xs))
				  | otherwise = multiplosDeN n (tail xs)

--multiplosDeNPM

-- quitarFull elimina todas las apariciones de a
quitarFull :: Integer -> [Integer] -> [Integer]
quitarFull a xs | length xs == 0 = []
		    	| a == head xs = (quitarFull a (tail xs))
				| otherwise = (head xs):(quitarFull a (tail xs))
			
quitarPrimeraAparicion :: Integer -> [Integer] -> [Integer]
quitarPrimeraAparicion a xs | length xs == 0 = []
		    				| a == head xs = tail xs
							| otherwise = (head xs):(quitarPrimeraAparicion a (tail xs))

hayRepetidos :: Eq a => [a] -> Bool
hayRepetidos xs | length xs == 0 = False
				| pertenece (head xs) (tail xs) = True
			    | otherwise = hayRepetidos (tail xs) 

eliminarRepetidos :: Eq a => [a] -> [a]
eliminarRepetidos xs | length xs == 0 = []
					 | pertenece (head xs) (tail xs) = eliminarRepetidos (tail xs)
					 | otherwise = (head xs):(eliminarRepetidos (tail xs))

maximo :: [Integer] -> Integer
maximo xs | length xs == 0 = 0
		  | (head xs) > (maximo (tail xs)) = head xs
		  | otherwise = maximo (tail xs)

minimo :: [Integer] -> Integer
minimo xs | length xs == 0 = 0
		  | length xs == 1 = head xs
		  | (head xs) < (minimo (tail xs)) = head xs
		  | otherwise = minimo (tail xs)


ordenar:: [Integer] -> [Integer]
ordenar xs | length xs == 0 = []
	   	   | (head xs) == minimo xs = (head xs):(ordenar (tail xs))
           | otherwise = (minimo (tail xs)):listaOrdenadaSinMinimo
			where listaOrdenadaSinMinimo = (ordenar ((head xs):(quitarPrimeraAparicion (minimo (tail xs)) (tail xs))))

