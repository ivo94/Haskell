---1)_a
esMin :: Char -> Bool

esMin x = elem x ['a'.. 'z']

--1)_b
letANat :: Char -> Integer

letANat x = sum[ b | ( a, b) <- lea x, ( x, b)== (a, b)]

lea :: Char  -> [(Char, Integer)]

lea x = zip ['a'..x] [0..25]

--1)_c
natALet :: Integer -> Char

natALet j = dame[ x | (x, y) <- reverlea j, ( x, j) == ( x, y)] 


reverlea :: Integer  -> [(Char, Integer)]

reverlea x = zip ['a'..'z'] [0..x]

dame :: [Char] -> Char

dame [j] = j

--1)_d
desplazar :: Integer -> Char -> Char
desplazar n a = if esMin a == False then a else desplazabien n a
 
desplazabien :: Integer -> Char -> Char
desplazabien n a 
 | letANat (a) + n <= 25 = natALet (letANat(a) + n)
 | letANat (a) + n >  25 = natALet ((mod (letANat(a) + n) 25) -1)
 
 -- no hay drama el n max es 25
 -- lo cambie un poco porq yo interpreto en el enunciado q si a es mayuscula te devuelve mayuscula, se lo consulta y fue 

codificar :: Integer -> String -> String
codificar n a  desplazar n (head a) : codificar n (tail a) 
 
 
