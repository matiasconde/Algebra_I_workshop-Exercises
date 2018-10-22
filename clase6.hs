-- PATTERN MATCHING

-- factorial 0       = 1
-- factorial (n + 1) = (n + 1) * factorial n
-- no es correcta porque si n=1 no encuentra match. 

factorial 0 = 1
factorial n = n*factorial (n-1)

--iguales :: Integer -> Integer -> Bool
--iguales x x = True
--iguales x y = False
-- no es correcta la definición porque hay conflicto de argumentos

yLogico :: Bool->Bool->Bool
yLogico True True = True
yLogico _ _ = False

oLogico :: Bool->Bool->Bool
oLogico False False = False
oLogico _ _ = True

implica :: Bool->Bool->Bool
implica True False = False
implica _ _ = True

sumaGaussiana :: Integer -> Integer
sumaGaussiana 0 = 0
sumaGaussiana n = n + sumaGaussiana (n-1)

algunoEsCero :: (Integer,Integer,Integer) -> Bool
algunoEsCero (a,b,c) = a*b*c==0

productoInterno :: (Float,Float) -> (Float,Float) -> Float
productoInterno ( x1 , y1 ) ( x2 , y2 ) = x1*x2 + y1*y2

sumaDivisoresHasta :: Integer->Integer->Integer
sumaDivisoresHasta n k | k==1 = 1
					   | mod n k == 0 = k + sumaDivisoresPuntoAnterior
					   | otherwise = sumaDivisoresPuntoAnterior
					   where sumaDivisoresPuntoAnterior = sumaDivisoresHasta n (k-1)

sumaDivisores :: Integer->Integer
sumaDivisores n = sumaDivisoresHasta n n 

esPrimo :: Integer -> Bool
esPrimo n = sumaDivisores n == n+1

esSumaDeDosPrimosHasta :: Integer -> Integer -> (Bool,Integer)
esSumaDeDosPrimosHasta n k | k==n = (False,-1)
	                       | (esPrimo (n-k)) && (esPrimo k) = (True,k)
						   | otherwise = esSumaDeDosPrimosHasta (n) (k+1)

-- (esPrimo k) && (esPrimo (n-k))
						   

esSumaDeDosPrimos :: Integer -> (Bool,Integer)
esSumaDeDosPrimos 1 = (False,-1)
esSumaDeDosPrimos n = esSumaDeDosPrimosHasta n 2

goldBachHasta :: Integer->Integer->Bool
goldBachHasta 0 0 = False
goldBachHasta 100 k = fst (esSumaDeDosPrimos k)
goldBachHasta n k = (fst (esSumaDeDosPrimos n))&&(goldBachHasta (n+2) k) --Tarda mucho si la defino hasta 2*10^18

sumaDigitosPositivos :: Integer -> Integer
sumaDigitosPositivos 0 = 0
sumaDigitosPositivos n = (mod n 10) + sumaDigitosPositivos (div n 10)

digitosIguales :: Integer -> Bool
digitosIguales 0 = True
digitosIguales n | (0<n)&&(n<10) = True
				 | (mod n 10) == (mod (div n 10) 10) = digitosIguales (div n 10)
				 | otherwise = False

collatz :: Integer -> Integer
collatz 1 = 1
collatz n | (mod n 2) == 0 = collatz  (div n 2) + 1
collatz n | (mod n 2) /= 0 = collatz (3*n + 1) + 1

maxTerminosCollatzHasta :: Integer -> (Integer,Integer) -- La primer componente me da la cantidad de términos de la sucesión, la segunda el número que la produce
maxTerminosCollatzHasta 1 = (1,1)
maxTerminosCollatzHasta n | collatz n > (fst maxTerminosCollatzAnteriores) = (collatz n,n)
						  | otherwise = maxTerminosCollatzAnteriores
						  where maxTerminosCollatzAnteriores = maxTerminosCollatzHasta (n-1)
