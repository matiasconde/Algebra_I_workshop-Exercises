signo n | n > 0 =1
	    | n==0 = 0
	    | otherwise = (-1)

absoluto_casera x | x>=0 = x
	              | x<0 = (-1)*x

absoluto x = x*(signo x)

maximo x y | x>=y = x
	       | otherwise = y

maximo3 x y z | z>=(maximo x y) = z
	          | otherwise = (maximo x y)

funcion3 :: Integer -> Integer -> Bool -> Bool
funcion3 x y b = b || (x>y)

funcion3_bis :: Integer->Integer->Bool->Bool
funcion3_bis x y b = (x>y)

doble :: Float -> Float
doble x = x + x

dist :: Float->Float->Float->Float->Float
dist x1 x2 y1 y2 = sqrt((x1-y1)**2+(x2-y2)**2)

esPar :: Integer-> Bool
esPar n = (mod n 2)==0

esMultiploDe :: Integer->Integer->Bool
esMultiploDe n m = (mod n m) == 0

triple :: Num a => a -> a
triple x = 3*x

quintuple x = 5*x

normaVectorial :: (Float,Float)->Float
normaVectorial x = sqrt((fst x)**2+(snd x)**2)

crearPar :: a->b->(a,b)
crearPar a b = (a,b)

invertir::(a,b)->(b,a)
invertir x = (snd x,fst x)

distanciaPuntos :: (Float,Float)->(Float,Float)->Float
distanciaPuntos x y = normaVectorial (fst x - fst y,snd x - snd y)

f1 :: Float -> (Float,Float,Float)
f1 x = (2*x,x**2,x-7)

f2 :: Integer -> Integer
f2 x | esPar x = div x 2
     | otherwise = x + 1

f :: Integer->Integer
f n | (mod n 6) == 0 = (div (n*n) 2)
    | otherwise = 3*n + 1

g :: (Integer,Integer)->Integer
g (n,m) = n*(m+1)

h :: (Integer,Integer)->Integer
h x = f (g x)



