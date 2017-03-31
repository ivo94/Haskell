--TP Taller de Algebra Primer Cuatrimestre 2015 
--Integrantes: Erika Ortiz, Pablo Chimiski, Ivo Begonja
--Turno Viernes

--1 a)
auxi2 :: Char -> [Char] -> Bool
auxi2 x y | length y == 0 =False
          | x==head y = True
          | x/=head y = auxi2 x (tail y)

esMin :: Char -> Bool		  
esMin x = auxi2 x ['a'..'z']

--1 b)
letANat :: Char -> Integer		  
letANat a = agarra a (zip ['a','b'..'z'] [0,1..25])

agarra :: Char -> [(Char,Integer)] -> Integer
agarra a b | a==fst(head b) = snd (head b)
           | a/=fst(head b) = agarra a (tail b)

--1 c)
natALet :: Integer -> Char
natALet a = agarra2 a (zip [0,1..25] ['a','b'..'z'])

agarra2 :: Integer -> [(Integer,Char)] -> Char
agarra2 a b 
 | a == fst (head b) = snd (head b)
 | a /= fst (head b) = agarra2 a (tail b) 
 
--1 d)
desplazar :: Integer -> Char -> Char
desplazar n a = if esMin a == False then a else desplazabien n a
 
desplazabien :: Integer -> Char -> Char
desplazabien n a 
 | n == 25 && a == 'z' = 'y'
 | letANat (a) + n <= 25 = natALet (letANat(a) + n)
 | letANat (a) + n >  25 = natALet ((mod (letANat(a) + n) 25) -1)
 
 
-- 1 e)
cantMinusc :: String -> Integer
cantMinusc x | length x==0 = 0
             | esMin (head x)==True = 1 + cantMinusc (tail x)
             | esMin (head x)==False = 0 + cantMinusc (tail x)
			 
-- 1 f)
contar :: Char -> String -> Integer
contar x a | length a ==0 = 0
           | x==head a = 1 + contar x (tail a)
           | x/=head a = contar x (tail a)

--2 		   
codificar :: Integer -> String -> String
codificar x a | length a == 1 = desplazar x (head a) : ""
              | esMin (head a)==True = desplazar x (head a) : codificar x (tail a)
              | esMin (head a)==False = head a : codificar x (tail a)	

--3			  
reversoletANat :: Char -> Integer
reversoletANat x = agarra x (zip ['a','b'..'z'] [25,24..0]) 

reversonatALet :: Integer -> Char
reversonatALet x = agarra2 x (zip [0,1..25] ['z','y'..'a']) 

desplazarIzq :: Integer -> Char -> Char
desplazarIzq n a = if esMin a == False then a else desplazabienIzq n a 

desplazabienIzq :: Integer -> Char -> Char
desplazabienIzq n a 
 | n == 25 && a == 'a' = 'b'
 | reversoletANat (a) + n <= 25 = reversonatALet (reversoletANat(a) + n)
 | reversoletANat (a) + n >  25 = reversonatALet ((mod (reversoletANat(a) + n) 25)-1)
 
decodificar :: Integer -> String -> String
decodificar x a 
 | length a == 1 = desplazarIzq x (head a) : ""
 | esMin (head a) == True = desplazarIzq x (head a) : decodificar x (tail a)
 | esMin (head a) == False = head a : decodificar x (tail a)
 
--4
contarfloat :: Char -> String -> Float
contarfloat x a | length a ==0 = 0
                | x==head a = 1 + contarfloat x (tail a)
                | x/=head a = contarfloat x (tail a)     
contaralf :: String -> [Char] -> [Float] 

contaralf x y  | length y == 1 = ((contarfloat (head y) x)*100) : [] 
               | length y /= 0 = ((contarfloat (head y) x)*100)/(longFloat (filter esMin x)) : contaralf x (tail y)                                                                             
																							 
paso :: String -> [Float]	 
 
paso x = contaralf x ['a'..'z'] 

frec :: String -> [Float]
frec x = paso x

longFloat :: [a] -> Float
longFloat [] = 0
longFloat (x:xs) = 1 + longFloat xs 

--5
rotar :: Integer -> [a] -> [a]                          
rotar x a | x>(long a) = rotar (mod x (long a))  a
          | x<(long a) = (drope x a) ++ (taki x a)       
          | x == long a = a     
taki :: Integer -> [a] -> [a] 
taki 0 _ = []
taki n [] = []		  
taki n (x: xs) = x : taki ( n -1) xs			
drope :: Integer -> [a] -> [a]
drope 0 l1 = l1
drope n [] = []
drope n (x: xs) = drope (n -1)  xs
long :: [a] -> Integer
long [] = 0
long (x:xs) = 1 + long xs 

--6

chi2 :: [Float] -> [Float] -> Float
chi2 a b 
 | length a > 0 && length b > 0 = (head a - head b)^2/(head b) + chi2 (tail a) (tail b)
 | otherwise = 0 

--7

distanciachi :: Integer -> String -> Float 
distanciachi n a = chi2 (rotar n (frec a)) frecalf 
 where frecalf = [12.52, 1.42, 4.67, 5.85, 13.67, 0.67, 1.01, 0.70, 6.24, 0.44, 0.01,4.96, 3.15, 6.70, 8.67, 2.51, 0.88, 6.86, 7.97, 4.62, 3.92, 0.90, 0.02, 0.22, 0.90, 0.52]

enlista :: Integer -> String -> [Float] 
enlista n a = distanciachi n a : [] 

concatena :: Integer -> String -> [Float] 
concatena n a 
 | n == 0 = []
 | n > 0 = enlista n a ++ concatena (n-1) a

rotaciones25 :: String -> [Float] 
rotaciones25 a = concatena 25 a   

reverso :: [a] -> [a] 
reverso [] = []
reverso xs = (reverso(tail xs) ) ++ [head xs]

rotacionesOrdenadas :: String -> [Float] 
rotacionesOrdenadas a = reverso (rotaciones25 a)

rotacionMinima :: String -> Float 
rotacionMinima x = minimo (rotacionesOrdenadas x)

minimo :: [Float] -> Float 
minimo x 
 | length x == 2 = min (head (x)) (head (tail (x)))
 | otherwise = min ((min (head (x))) (head (tail (x)))) (minimo (tail x))

posicion :: Float -> [Float] -> Integer 
posicion x a
 | x == head a = 1
 | x /= head a = 1 + posicion x (tail a)
 
posiciondelMinimo :: String -> Integer 
posiciondelMinimo a = posicion (rotacionMinima a) (rotacionesOrdenadas a)

descifrar :: String -> String 
descifrar a = decodificar (posiciondelMinimo a) a 