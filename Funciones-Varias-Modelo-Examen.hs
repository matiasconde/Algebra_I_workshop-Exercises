-- Funciones 
-- 1. MENOR LEXICOGRÁFICO DE TUPLA TRIPLE (pregunta si la primera es menor que la segunda)
menorLex :: (Float,Float,Float) -> (Float,Float,Float) -> Bool
menorLex (x1,y1,z1) (x2,y2,z2) | x1>x2 = False
							   | x1<x2 = True
							   | (x1==x2)&&(y1>y2) = False
							   | (x1==x2)&&(y1<y2) = True
							   | (x1==x2)&&(y1==y2)&&(z1>z2) = False
							   | (x1==x2)&&(y1==y2)&&(z1<z2) = True 
							   | otherwise = False

--2. Suma Fibonnaci
fibo :: Integer -> Integer  --Auxiliar
fibo 0 = 1
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

sumaFiboN :: Integer -> Integer --Función Principal
sumaFiboN 1 = 2
sumaFiboN n = fibo n + sumaFiboN (n-1)

--3. Averiguar si un número es defectivo (la suma de sus divisores propios es menor estricto que el número)
sumaDivisoresHasta :: Integer -> Integer -> Integer --Auxiliar
sumaDivisoresHasta n k | k==1 = 1
					   | (mod n k)==0 = k + sumaDivisoresHasta n (k-1)
				       | otherwise = sumaDivisoresHasta n (k-1)

esDefectivo :: Integer -> Bool --Función Principal
esDefectivo 1 = False
esDefectivo n = (sumaDivisoresHasta n (n-1)) < n

--4. Maxima distancia entre dos elementos consecutivos de una lista. Asumir que al menos la lista tiene 2 elementos. 

mD :: [Integer] -> Integer 
mD (x:y:[]) = abs (x-y)
mD (x:y:ys) = max (abs (x-y)) (mD (y:ys))

--4. BIS Forma auxiliar complicada: 

selecElemI :: [Integer] -> Integer -> Integer --Auxiliar
selecElemI (x:xs) 1 = x
selecElemI (x:xs) n = selecElemI xs (n-1)

modulo :: Integer -> Integer --Auxiliar
modulo 0 = 0
modulo n | n>0 = n
		 | otherwise = (-1)*n

distConsecInvHasta :: [Integer] -> Integer -> [Integer] --Auxiliar
distConsecInvHasta a k | k==2 = [modulo ((selecElemI a 1) - (selecElemI a 2))]
				    | k>2 = (modulo ((selecElemI a k) - (selecElemI a (k-1)))):(distConsecInvHasta a (k-1))

invertirLista :: [Integer] -> [Integer] --Auxiliar
invertirLista [] = []
invertirLista (x:xs) = (invertirLista xs)++[x]

distConsecHasta :: [Integer] -> Integer -> [Integer] --Auxiliar
distConsecHasta [] k = []
distConsecHasta a k = invertirLista (distConsecInvHasta a k)

distConsec :: [Integer] -> [Integer] --Auxiliar
distConsec a = distConsecHasta a (toInteger (length a))

maximoLista :: [Integer] -> Integer --Auxiliar
maximoLista [] = 0 --por convención
maximoLista (x:xs) | toInteger (length (x:xs)) == 0 = 0
			       | x> (maximoLista xs) = x
				   | otherwise = maximoLista xs

maximaDistancia :: [Integer] -> Integer --Función Principal
maximaDistancia a = maximoLista (distConsec a)

--5. Realizar la función comprimir que dada una lista de enteros, devuelva una lista de tuplas en donde la primer coordenada es el entero que aparece y la segunda la frecuencia de este entero en forma consecutiva. 
 
replace :: [Integer] -> [(Integer,Integer)] --Auxiliar 
replace [] = []
replace (x:xs) = (x,1):(replace xs)

ctrc :: [(Integer,Integer)] -> [(Integer,Integer)] --Auxiliar
ctrc [] = []
ctrc ((x1,x2):[]) = [(x1,x2)]
ctrc ((x1,x2):y:ys) | x1 == fst (head (ctrc (y:ys))) = (x1,x2 + snd (head (ctrc (y:ys)))):(tail(ctrc (y:ys)))
				    | otherwise = (x1,x2):(ctrc (y:ys)) 

comprimir :: [Integer] -> [(Integer,Integer)] --Funcion Principal
comprimir a = ctrc (replace a)

--5. Comrpimir segunda forma
ctrc2 :: [(Integer,Integer)] -> [(Integer,Integer)] --Auxiliar
ctrc2 [] = []
ctrc2 ((x1,x2):[]) = [(x1,x2)]
ctrc2 ((x1,x2):(y1,y2):ys) | x1==y1 = ctrc2 ((x1,x2+y2):ys)
						   | otherwise = (x1,x2):(ctrc2 ((y1,y2):ys))

comprimir2 :: [Integer] -> [(Integer,Integer)] --Funcion Principal
comprimir2 a = ctrc2 (replace a)

