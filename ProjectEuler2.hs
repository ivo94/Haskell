--Problema 1
multiplosde3 :: Integer -> [Integer] 
multiplosde3 n 
 | n == 0 = []
 | mod n 3 == 0 = (n) : multiplosde3 (n-3) 
 | mod n 3 /= 0 = multiplosde3 (n-1)
 
multiplosde5 :: Integer -> [Integer]
multiplosde5 n 
 | n == 0 = []
 | mod n 5 == 0 && mod n 3 /= 0 = (n) : multiplosde5 (n-5) 
 | mod n 5 /= 0 && mod n 3 == 0 = multiplosde5 (n-1)
 | mod n 5 /= 0 && mod n 3 /= 0 = multiplosde5 (n-1)
 | mod n 5 == 0 && mod n 3 == 0 = multiplosde5 (n-1)
 
problem1 :: Integer -> Integer
problem1 n = sum (multiplosde3 (n-1)) + sum (multiplosde5 (n-1))
--Problema 2
fibonacci :: Integer -> Integer
fibonacci n 
 | n == 1 = 1 
 | n == 2 = 2
 | n > 2 = fibonacci (n-1) + fibonacci (n-2)

fibonacciEnlistados :: Integer -> [Integer]
fibonacciEnlistados n 
 | n == 0 = []
 | n > 0 = fibonacci (n) : fibonacciEnlistados (n-1)

fibonacciEnlistadosPares :: Integer -> [Integer]
fibonacciEnlistadosPares n = filter (p) (fibonacciEnlistados n)
 where p n =  mod n 2 == 0 
 
fibonacciEnlistadosParesMenoresa4millones :: Integer -> [Integer]
fibonacciEnlistadosParesMenoresa4millones n = filter (q) (fibonacciEnlistadosPares n)
 where q n = n < 4000000
 
sumadefibonaccis :: Integer -> Integer 
sumadefibonaccis n 
 | n == 0 = 0 
 | mod (fibonacci (n)) 2 == 0 && fibonacci (n) <= 4000000 = fibonacci (n) + sumadefibonaccis (n-1) 
 | mod (fibonacci (n)) 2 == 0 && fibonacci (n) > 4000000 = 0
 | mod (fibonacci (n)) 2 /= 0 && fibonacci (n) <= 4000000 = sumadefibonaccis (n-1) 
 | mod (fibonacci (n)) 2 /= 0 && fibonacci (n) > 4000000 = 0 

problem2 :: Integer -> Integer
problem2 n = sumadefibonaccis 32 --fibonacci de 32 es el ultimo termino < 4000000

--Problema 3 usar raiz cuadrada para bajar magnitud
ultimo :: Integer -> Integer -> Integer
ultimo n m
 | mod m (n-1) == 0 = n-1
 | mod m (n-1) /= 0 = ultimo (n-1) m 
 
ultimoDivisorPrimo :: Integer -> Integer
ultimoDivisorPrimo n
 | ultimo n n == 1 = n 
 | ultimo n n /= 1 = ultimoDivisorPrimo (ultimo n n)
 
entero :: Float -> Bool
entero n 
 | elem n [1..] = True
 | otherwise = False

ultimoDivisor :: Integer -> Integer
ultimoDivisor n = ultimo n n

esPrimo :: Integer -> Bool
esPrimo n 
 | mod (2^(n-1)) n == 1 = True
 | otherwise = False

ultimoDivisorPrimoB :: Integer -> Integer -> Integer
ultimoDivisorPrimoB n m
 | mod n m == 0 && esPrimo(m) = m 
 | otherwise = ultimoDivisorPrimoB n (m+1)
 
ultimoDivisorPrimoPosta :: Integer -> Integer
ultimoDivisorPrimoPosta n = ultimoDivisorPrimoB n 6858

--la respuesta del problema 3 de ProjectEuler es 6857


prime_factors_of :: Integer -> [Integer]
prime_factors_of 0 = []
prime_factors_of 1 = []
prime_factors_of n = f : prime_factors_of(n `div` f)   where f = head $ filter (\d -> n `mod` d == 0) [2..n]

 


 