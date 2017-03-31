factorial :: Integer -> Integer
factorial n 
 | n == 0 = 1
 | n /= 0 = n * factorial (n-1)
y :: Integer -> Integer -> Float
y a b = 1 / (factorial (a-b) * factorial b) 
