factorial :: Double -> Double
factorial n
 | n == 0 = 1
 | otherwise = n * factorial (n-1)
combinatorio :: (Double, Double) -> Double
combinatorio (n, m) = (factorial (n-1) / (factorial (n-m) * factorial (m-1))) + (factorial (n-1) / (factorial (n-1-m) * factorial m))
suma :: [Integer] -> Integer
suma n
 | length n == 0 = 0
 | otherwise = head n + suma (tail n)
sumatoria :: Integer -> Integer 
sumatoria n
 | n == 0 = 0
 | otherwise = n + sumatoria (n-1)
esImpar :: Integer -> Bool
esImpar x 
 | mod x 2 == 1 = True
 | otherwise = False
fibonacci :: Integer -> Integer
fibonacci n
 | n == 0 = 0 
 | n == 1 = 1
 | n > 1 = fibonacci (n-1) + fibonacci (n-2)
sumaImpares :: Integer -> Integer
sumaImpares n
 | n == 0 = 0
 | n > 0 = (2 * n - 1) + sumaImpares (n-1)
sumaCuadradosImpares :: Integer -> Integer
sumaCuadradosImpares n
 | n == 0 = 0
 | n > 0 =(2 * n - 1)^2 + sumaCuadradosImpares (n-1)
x :: Integer -> Integer
x 0 = 0
sumatoriaimpar :: Integer -> Integer
sumatoriaimpar n
 | n < 0 = 0
 | n == 0 = 0
 | mod (n-1) 2 == 1 = n-1 + sumatoriaimpar (n-1)
 | mod (n-1) 2 == 0 = sumatoriaimpar (n-1)
sumaCIMA :: Integer -> Integer 
sumaCIMA n = sum (filter (p)(takeWhile (<n) [1,3..]))
 where p x = x^2 < n 

 


 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 