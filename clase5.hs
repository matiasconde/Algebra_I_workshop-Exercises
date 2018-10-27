factorial :: Integer->Integer
factorial n | n>0 = n*(factorial (n-1))
	        | n==0 = 1  -- caso base 

sumaLosPrimerosNImpares :: Integer -> Integer
sumaLosPrimerosNImpares n
	| n == 1 = 1
 	| n > 1 = n_esimoimpar + sumaLosPrimerosNImpares (n-1)
	where n_esimoimpar = (2*n - 1) -- where es un alias. 

eAprox :: Integer -> Float
eAprox n | n==0 = 1
         | n>0 = (eAprox (n-1)) + 1/(fromInteger (factorial n))

e :: Float
e = eAprox 100

parteEntera :: Float -> Integer 
parteEntera a | a-1 == 0 = 1
	          | a-1 < 0 = 0
              | a-1 > 0 = parteEntera (a-1) + 1

parteEnteraConNegativos :: Float -> Integer
parteEnteraConNegativos a | a < 0 = -parteEntera (-a) - 1
	                      | otherwise = parteEntera a

division :: Integer -> Integer -> (Integer,Integer)
division a d | d > a = (0,a)
	         | otherwise = (1+cocienteAnterior , resto)
	     	 where 
		     rec = division (a-d) d
		     cocienteAnterior = fst rec
		     resto = snd rec			

divisionConNegativos :: Integer -> Integer -> (Integer,Integer)
divisionConNegativos a d | a < 0 = (cocienteNegativo, a-cocienteNegativo*d)
		             	 | a >= 0 = division a d
			             where 
						 cocienteNegativo = -fst (division (-a) d) - 1


sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta n k | n < 0 = -(sumaDivisoresHasta (-n) k)
					   | k==1 = 1 
				       | snd (division n k) == 0 = k + sumaDivisoresPuntoAnterior
		               | otherwise = sumaDivisoresPuntoAnterior
					   where sumaDivisoresPuntoAnterior = sumaDivisoresHasta n (k-1) 
	     
sumaDivisores :: Integer -> Integer
sumaDivisores a | a >= 0 = sumaDivisoresHasta a a
			    | a < 0 = sumaDivisoresHasta a (-a)

menorDivisorDesde :: Integer-> Integer -> Integer --Si empezamos en 1, el menor divisor será 1. 
menorDivisorDesde a k | a == k = a
					  | mod a k == 0 = k
					  | otherwise = menorDivisorDesde a (k+1)

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2 

esPrimo :: Integer -> Bool
esPrimo a | mod a 2 == 0 = False  -- Le agregué esto para descartar números pares grandes de una. 
		  | a==0 = False 
		  | a<0 = esPrimo (-a)
		  | sumaDivisores a == a+1 = True
     	  | otherwise = False

esPrimo2 :: Integer -> Bool
esPrimo2 n | n==1 = False
		   | n>1 = (menorDivisor n) == n

sumaSimple :: Integer -> Integer -> Integer
sumaSimple i m | m==1 = i
		       | m > 1 = i^m + (sumaSimple i (m-1))

sumaDoble n m | n == 1 = sumaSimple 1 m
			  | n > 1 = (sumaSimple n m) + (sumaDoble (n-1) m)

sumaPotencias :: Integer->Integer->Integer->Integer
sumaPotencias q n m = (sumaSimple q n)*(sumaSimple q m)

sumaFracciones :: Float -> Float -> Float
sumaFracciones q n | n == 1 = q			
				   | otherwise = q/n + sumaFracciones q (n-1)

sumaRacionales :: Float->Float->Float
sumaRacionales m n = sumaFracciones (m*(m+1)/2) n

sumaRacionales2 :: Float->Float->Float
sumaRacionales2 m n = (m*(m+1)/2)*(sumaFracciones 1 n) --Obvio da igual pues saco factor común. 
