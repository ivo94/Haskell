auxi2 :: Char -> [Char] -> Bool
auxi2 x y | length y == 0 =False
          | x==head y = True
          | x/=head y = auxi2 x (tail y)

auxi :: Char -> Bool		  
auxi x = auxi2 x ['a'..'z']

esMin :: Char -> Bool
esMin x = auxi x 
--4
contarfloat :: Char -> String -> Float
contarfloat x a 
 | length a == 0 = 0 
 | x == head a = 1 + contarfloat x (tail a)
 | x /= head a = contarfloat x (tail a)

contaralf :: String -> [Char] -> [Float]
contaralf x y
 | length y == 1 = ((contarfloat (head y) x)*100) : []                                   
 | length y /= 0 = ((contarfloat (head y) x)*100) / (length (filter esMin x)) : contaralf x (tail y)
 
 
todo1 :: Char -> Integer
todo1 x = sum[ b | ( a, b) <- leatodo1 x, ( x, b)== (a, b)]

leatodo1 :: Char  -> [(Char, Integer)]
leatodo1 x = zip ['a'..'z'] [1..1]
 
