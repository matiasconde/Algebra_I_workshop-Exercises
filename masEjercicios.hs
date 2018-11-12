--2.
sumatoria :: Integer -> Integer 
sumatoria 1 = 1
sumatoria n = (2*n-1)^2 + sumatoria (n-1)

--3.
todosIguales :: [Integer] -> Bool
todosIguales [] = True
todosIguales (x:[]) = True
todosIguales (x:y:ys) = (x==y)&&(todosIguales (y:ys))

todosDistintos :: [Integer] -> Bool
todosDistintos [] = True
todosDistintos (x:[]) = True
todosDistintos (x:y:ys) = (x==y)&&(todosIguales (y:ys))

--4.
quitar :: [Integer] -> Integer -> [Integer]
quitar [] _ = []
quitar (x:xs) n | x==n = quitar xs n
			    | otherwise = x:(quitar xs n)

sacarTodos :: [Integer] -> [Integer] -> [Integer]
sacarTodos a [] = a
sacarTodos a (x:xs) = sacarTodos (quitar a x) xs

--1.
sumar2 :: (Integer,Integer,Integer) -> Integer -> Bool
sumar2 (a,b,c) n | (a+b)==n = True
				 | (a+c)==n = True
				 | otherwise = (b+c)==n

--5. 
fInt :: Int -> Float
fInt x = fromInteger (toInteger x)

sumarNotas :: [Float] -> Float
sumarNotas [] = 0
sumarNotas (x:xs) = x + sumarNotas xs

prom :: [Float] -> Float
prom [] = 0
prom (x:xs) = (sumarNotas (x:xs))/(fInt (length (x:xs)))

seleccionNotasAl :: [(Integer,Float)] -> Integer -> [Float]
seleccionNotasAl [] n = []
seleccionNotasAl ((x,y):ys) n | x==n = [y]++(seleccionNotasAl ys n)
							  | otherwise = (seleccionNotasAl ys n)

promedioDe :: [(Integer,Float)] -> Integer -> Float
promedioDe a n = prom (seleccionNotasAl a n)


