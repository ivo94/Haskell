reversoletANat :: Char -> Integer
reversoletANat x = sum[ b | ( a, b) <- reversoLea x, ( x, b)== (a, b)]
reversoLea :: Char  -> [(Char, Integer)]
reversoLea x = zip ['a'..x] [25,24..0]
reversonatALet :: Integer -> Char
reversonatALet j = dame[ x | (x, y) <- invertirlea j, ( x, j) == ( x, y)] 
invertirlea :: Integer  -> [(Char, Integer)]
invertirlea x = zip ['a'..'z'] [25,24..0]
desplazarIzq :: Integer -> Char -> Char
desplazarIzq n a = if esMin a == False then a else desplazabienIzq n a
desplazabienIzq :: Integer -> Char -> Char
desplazabienIzq n a 
 | reversoletANat (a) + n <= 25 = reversonatALet (reversoletANat(a) + n)
 | reversoletANat (a) + n >  25 = reversonatALet ((mod (reversoletANat(a) + n) 25)-1)
decodificar :: Integer -> String -> String
decodificar x a 
 | length a == 1 = desplazarIzq x (head a) : ""
 | esMin (head a) == True = desplazarIzq x (head a) : decodificar x (tail a)
 | esMin (head a) == False = ' ' : decodificar x (tail a)