
posicion :: Integer -> [Integer] -> Integer 
posicion x a
 | x == head a = 1
 | x /= head a = 1 + posicion x (tail a)

elemento :: Integer -> [Integer] -> Integer
elemento x a = 