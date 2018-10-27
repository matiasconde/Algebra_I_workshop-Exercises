-- RECURSIÓN:
-- expresión con llamado recursivo + caso base (no tiene llamado recursivo)
factorial :: Integer->Integer
factorial n | n>0 = n*(factorial (n-1))
	        | n==0 = 1  -- caso base 

sc :: Integer->Integer
sc n | n==0 = 0  -- caso base
     | otherwise = sc (n-1) + n*n

fibo :: Integer->Integer
fibo n | n==0 = 0 -- caso base 1
       | n==1 = 1 -- caso base 2
       | otherwise = fibo (n-1) + fibo (n-2)


ej1 :: Integer -> Integer 
ej1 n | n==1 = 2
	  | n>1 = 2*(n-1)*(ej1 (n-1)) + (factorial (n-1))*(2^n)

ej2 :: Integer -> Integer 
ej2 n | n==1 = 1
      | n==2 = 2
      | n>2 = (n-2)*(ej2 (n-1)) + 2*(n-1)*(ej2 (n-2))

esPar :: Integer-> Bool
esPar n = (mod n 2)==0

ej3 :: Integer->Integer
ej3 n | n==1 = -3
      | n==2 = 6
      | (esPar n) = (ej3 (n-1)) + 2*(ej3 (n-2)) + 9
      | not(esPar n) = -(ej3(n-1))-3

f1 :: Integer->Integer
f1 n | n==0 = 1
     | n>0 = 2^n + (f1 (n-1))
 
f2 :: Integer->Float->Float
f2 n q | n == 1 = q
       | n>1 = q^n + (f2 (n-1) q)

f3 :: Integer->Float->Float
f3 n q | n==0 = 1
       | n>0 = q^(2*n) + (f3 (n-1) q)

f4 :: Integer->Float->Float
f4 n q = q^n+(q^n)*(f2 n q) 

esPar2 :: Integer -> Bool
esPar2 n | n==0 = True
	     | n>0 = not(esPar2 (n-1))   

esPar3 :: Integer -> Bool
esPar3 n | n==0 = True
	     | n==1 = False
	     | n>1 = esPar3 (n-2)

esMultiplode3 :: Integer -> Bool 
esMultiplode3 n | n<3 = False 
		| n==3 = True
  	   	| otherwise = esMultiplode3 (n-3)

sumaImpares :: Integer -> Integer
sumaImpares n | n==0 = 0
	          | (esPar2 n) = 2*n-1 + sumaImpares (n-1)
 	          | not(esPar2 n) = 2*n-1 + sumaImpares (n-1)

doblefactorial :: Integer->Integer
doblefactorial n | n==0 = 1
	             | n==1 = 1
		         | otherwise = n*doblefactorial (n-2)

recur_inf_neg :: Integer->Integer --esta función termina si se ejecuta con positivos y no termina si se ejecuta con negativos
recur_inf_neg n | n==0 = 1
         		| otherwise = recur_inf_neg (n-1)
				
pow2 n = 2^n
	        

