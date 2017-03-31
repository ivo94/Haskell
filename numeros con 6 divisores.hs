--clase 9
divParcial :: Int -> Int -> [Int]
divParcial n 1 = [1]
divParcial n m 
 | mod n m == 0 = m : divParcial n (m -1)
 | otherwise = divParcial n (m -1)
 
divisores :: Int -> [Int]
divisores n = divParcial n n

numeros6div :: Int -> [Int]
numeros6div n = filter (p) [1..n] 
 where p n = length (divisores n) == 6
 
sumaDivisores :: Int -> Int
sumaDivisores n = sum (divisores(n))

esCuboPParcialmente :: Int -> Int -> Bool
esCuboPParcialmente n m
 | n == 0 = True 
 | m == 0 = False
 | m^3 == n = True
 | m^3 /= n = esCuboPParcialmente n (m-1)
 
esCuboP :: Int -> Bool
esCuboP n = esCuboPParcialmente n n

esCuboNParcialmente :: Int -> Int -> Bool
esCuboNParcialmente n m
 | n == 0 = True 
 | m == 0 = False
 | m^3 == n = True
 | m^3 /= n = esCuboNParcialmente n (m+1)

esCuboN :: Int -> Bool
esCuboN n = esCuboNParcialmente n n

esCubo :: Int -> Bool
esCubo n 
 | n >= 0 = esCuboP (n)
 | n < 0 = esCuboN (n)

sumatoria :: Integer -> Integer
sumatoria n 
	| n == 0 = 0
	| otherwise = n + sumatoria(n-1)
	
sumatoriaPosta :: Integer -> Integer
sumatoriaPosta n
	| mod n 2 == 0 = (n+1)*(div n 2)
	| mod n 2 /= 0 = (n+1)*(div n 2)+((div n 2)+1)