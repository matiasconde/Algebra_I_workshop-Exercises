unidades :: Integer -> Integer
unidades a = mod a 10

sumaUnidades3:: Integer -> Integer -> Integer -> Integer
sumaUnidades3 a b c = (mod a 10) + (mod b 10) + (mod c 10)

esPar :: Integer-> Bool
esPar n = (mod n 2)==0

esImpar :: Integer-> Bool
esImpar n = (mod n 2)/=0

todosImpares :: Integer -> Integer -> Integer -> Bool
todosImpares a b c =  (esImpar a)&&(esImpar b)&&(esImpar c)

todosImpares_v2 :: Integer -> Integer -> Integer -> Bool
todosImpares_v2 a b c = mod (a+b+c) 2 /=0
-- pensar todosImpares mas matematicamente. 		   

alMenosUnImpar :: Integer -> Integer -> Integer -> Bool
alMenosUnImpar a b c = (esImpar a)||(esImpar b)||(esImpar c)

alMenosDosImpares :: Integer -> Integer -> Integer -> Bool
alMenosDosImpares a b c | (esImpar a) && (esImpar b) = True
			| (esImpar b) && (esImpar c) = True
 			| (esImpar a) && (esImpar c) = True
			| otherwise = False

alMenosDosPares :: Integer->Integer->Integer->Bool
alMenosDosPares a b c | (esPar a) && (esPar b) = True
		      | (esPar b) && (esPar c) = True
 		      | (esPar a) && (esPar c) = True
		      | otherwise = False

alMenosUnMultiplo :: Integer->Integer->Integer->Bool
alMenosUnMultiplo a b c | mod a c ==0 = True
			| mod b c ==0 = True
			| otherwise = False

r1 :: Integer->Integer->Bool
r1 a b | ((mod a 2)==0) && ((mod b 2)==0) || ((mod a 2)/=0) && ((mod b 2)/=0) = True
       | otherwise = False

r2 :: Integer->Integer->Bool
r2 a b = mod (2*a+3*b) 5 == 0

r3 :: Integer->Integer->Bool
r3 a b = (unidades a /= unidades b)&&(unidades b /= unidades (a*b))&&(unidades a /= unidades a*b)

equivalencia9 :: Float->Float->Bool
equivalencia9 x y = (x<3)&&(y<3)

equivalencia10 :: Float->Float->Bool
equivalencia10 x y | (x<3)&&(y<3) = True
		   		   | ((x>=3)&&(y>=3))&&((x<7)&&(y<7)) = True 
		           | (x>=7)&&(y>=7) = True
                   | otherwise = False

rel11_1 :: (Float,Float)->(Float,Float)->Bool
rel11_1 v1 v2 = (fst v1)/(fst v2) == (snd v1)/(snd v2)

rel11_2 :: (Integer,Integer)->(Integer,Integer)->Bool
rel11_2 v1 v2 | ((fst v1/=0) && (fst v2/=0)) || ((snd v1/=0) && (snd v2/=0)) = (div (fst v1) (fst v2)) == (div (snd v1) (snd v2))
	          | otherwise = False

rel11_3 :: (Integer,Integer)->(Integer,Integer)->Bool
rel11_3 v1 v2 | ((fst v1/=0) && (fst v2/=0)) || ((snd v1/=0) && (snd v2/=0)) = (div (fst v1) (fst v2)) == (div (snd v1) (snd v2))
	          | otherwise = False

