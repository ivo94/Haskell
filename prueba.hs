--1,a)

auxi2 :: Char -> [Char] -> Bool
auxi2 x y | length y == 0 =False
          | x==head y = True
          | x/=head y = auxi2 x (tail y)

auxi :: Char -> Bool		  
auxi x = auxi2 x ['a'..'z']

esMin :: Char -> Bool
esMin x = auxi x 

--1.b)

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
 
-- 1.e

cantMinusc :: String -> Integer
cantMinusc x | length x==0 = 0
             | esMin (head x)==True = 1 + cantMinusc (tail x)
             | esMin (head x)==False = 0 + cantMinusc (tail x)
			 
-- 1.f

contar :: Char -> String -> Integer
contar x a | length a ==0 = 0
           | x==head a = 1 + contar x (tail a)
           | x/=head a = contar x (tail a)		   -- si "a" es mayuscula que pasa?
                                                   --funciona igual ya que no esta pidiendo que x sea minuscula  

--2 
		   
codificar :: Integer -> String -> String
codificar x a | length a == 1 = desplazar x (head a) : ""
              | esMin (head a)==True = desplazar x (head a) : codificar x (tail a)
              | esMin (head a)==False = ' ' : codificar x (tail a)				   
		   -- que pasa si en el string hay una letra mayuscula? hay q preguntar por ese caso
		   
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