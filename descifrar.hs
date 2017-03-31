auxi2 :: Char -> [Char] -> Bool
auxi2 x y | length y == 0 =False
          | x==head y = True
          | x/=head y = auxi2 x (tail y)

auxi :: Char -> Bool		  
auxi x = auxi2 x ['a'..'z']

esMin :: Char -> Bool
esMin x = auxi x 

dame :: [Char] -> Char
dame [j] = j

--parte de decodificar

reversoletANat :: Char -> Integer
reversoletANat x = sum[ b | ( a, b) <- reversoLea x, ( x, b)== (a, b)] -- hace lo mismo que letANat pero le asigna el 25 a la a y el 0 a la z (utilizando la la funcion auxiliar equivalente)

reversoLea :: Char  -> [(Char, Integer)]
reversoLea x = zip ['a'..x] [25,24..0] 

reversonatALet :: Integer -> Char
reversonatALet j = dame[ x | (x, y) <- invertirlea j, ( x, j) == ( x, y)] -- es como natALet pero el 0 devuelve la z y el 25 la a (utilizando la la funcion auxiliar equivalente)

invertirlea :: Integer  -> [(Char, Integer)]
invertirlea x = zip ['a'..'z'] [25,24..x]

desplazarIzq :: Integer -> Char -> Char
desplazarIzq n a = if esMin a == False then a else desplazabienIzq n a -- es como desplazar pero ahora lo hace hacia la izquierda, ejemplo: desplazarIzq 1 'a' = z

desplazabienIzq :: Integer -> Char -> Char
desplazabienIzq n a 
 | reversoletANat (a) + n <= 25 = reversonatALet (reversoletANat(a) + n)
 | reversoletANat (a) + n >  25 = reversonatALet ((mod (reversoletANat(a) + n) 25)-1)
 
decodificar :: Integer -> String -> String
decodificar x a 
 | length a == 1 = desplazarIzq x (head a) : ""
 | esMin (head a) == True = desplazarIzq x (head a) : decodificar x (tail a)
 | esMin (head a) == False = ' ' : decodificar x (tail a)
 
--parte de frecuencia

contarfloat :: Char -> String -> Float
contarfloat x a | length a ==0 = 0
                | x==head a = 1 + contarfloat x (tail a)
                | x/=head a = contarfloat x (tail a)     -- tuve que redefinir contar para que devuelva un float, porque sino devolvia un error de tipo la funcion contaralf
contaralf :: String -> [Char] -> [Float] --esta funcion se fija cuantas veces aparece la letra en el string y hace un promedio, despues aplica recursion para ir metiendola en una lista! (Si la letra no aparece es igual a. 

contaralf x y  | length y == 1 = ((contarfloat (head y) x)*100) : [] -- si length y == 1 entonces no es necesario dividir. Si la letra esta en el string entonces da 100 y si no da 0.
               | length y /= 0 = ((contarfloat (head y) x)*100)/(longFloat (filter esMin x)) : contaralf x (tail y) --solucionado lo de length                                                                             
																							 
paso :: String -> [Float]	 
 
paso x = contaralf x ['a'..'z'] -- una auxiliar con una lista char del alfabeto!

frec :: String -> [Float]
frec x = paso x

longFloat :: [a] -> Float
longFloat [] = 0
longFloat (x:xs) = 1 + longFloat xs 

--parte de rotar 

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

--chi cuadrado

chi2 :: [Float] -> [Float] -> Float
chi2  xs ys = sum[((x-y)^2)/y | (x, y) <- zip xs ys, length xs == length ys ]

-- descifrar

distanciachi :: Integer -> String -> Float -- calcula la distancia chi-cuadrado entre una rotacion n realizada a frec de un string y la lista de frecuencias de aparicion de las letras 
distanciachi n a = chi2 (rotar n (frec a)) frecalf 
 where frecalf = [12.52, 1.42, 4.67, 5.85, 13.67, 0.67, 1.01, 0.70, 6.24, 0.44, 0.01,4.96, 3.15, 6.70, 8.67, 2.51, 0.88, 6.86, 7.97, 4.62, 3.92, 0.90, 0.02, 0.22, 0.90, 0.52]

enlista :: Integer -> String -> [Float] -- coloca el valor de distanciachi en una lista
enlista n a = distanciachi n a : [] 

concatena :: Integer -> String -> [Float] --concatena los valores de distancias-chi en una lista 
concatena n a 
 | n == 0 = []
 | n > 0 = enlista n a ++ concatena (n-1) a

rotaciones25 :: String -> [Float] --pone todas las distancias chi cuadrado entre las 25 posibles rotaciones de frec de un string y la lista de frecuencias de aparicion en una sola lista
rotaciones25 a = concatena 25 a   -- en este caso el primer elemento de la lista resultante corresponde a la distancia chi cuadrado entre la rotacion 25 de frec de un string y la lista de frecuencias de aparicion

reverso :: [a] -> [a] 
reverso [] = []
reverso xs = (reverso(tail xs) ) ++ [head xs]

rotacionesOrdenadas :: String -> [Float] --es como rotaciones25 pero invierte la lista de modo que el primer elemento corresponda a la rotacion 1
rotacionesOrdenadas a = reverso (rotaciones25 a)

rotacionMinima :: String -> Float --identifica el valor minimo de las 25 distancia chi-cuadrado calculadas
rotacionMinima x = minimo (rotacionesOrdenadas x)

minimo :: [Float] -> Float --identifica el elemento mas chico en una lista
minimo x 
 | length x == 2 = min (head (x)) (head (tail (x)))
 | otherwise = min ((min (head (x))) (head (tail (x)))) (minimo (tail x))

posicion :: Float -> [Float] -> Integer --identifica la posicion que ubica un elemento en una lista
posicion x a
 | x == head a = 1
 | x /= head a = 1 + posicion x (tail a)
 
posiciondelMinimo :: String -> Integer --identifica la posicion que ubica el chi-cuadrado minimo en los 25 chi-cuadrados calculados
posiciondelMinimo a = posicion (rotacionMinima a) (rotacionesOrdenadas a)

descifrar :: String -> String 
descifrar a = decodificar (posiciondelMinimo a) a 