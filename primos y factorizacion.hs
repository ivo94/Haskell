divParcial :: Integer -> Integer -> [Integer]
divParcial n 1 = [1]
divParcial n m 
 | mod n m == 0 = m : divParcial n (m-1)
 | otherwise = divParcial n (m-1)
 
divisores :: Integer -> [Integer]
divisores n = divParcial n n

esPrimo :: Integer -> Bool
esPrimo n 
 | length (divisores n) == 2 = True
 | otherwise = False
 
productoriaDivisores :: Integer -> Integer
productoriaDivisores n = product (divisores n)

factorizar :: Integer -> [Integer]
factorizar n = filter (q)(filter (p) (takeWhile (<=n) (divisores n)))
 where p x = esPrimo x  
 where q x = 
 
sumaDivisores :: Integer -> Integer
sumaDivisores n = sum (divisores(n))