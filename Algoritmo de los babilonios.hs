raizcuadradaprima :: Double -> Double -> Integer -> Double -- a es el numero cuya raiz cuadrada se quiere estimar, 
										             -- b es el estimador
													 -- c es la cantidad de iteraciones que se realizan
													
raizcuadradaprima a b 0 = b
raizcuadradaprima a b c = raizcuadradaprima a ((b+(a/b))/2) (c-1) -- a es la primera estimacion de la raiz cuadrada
														  
raizcuadrada :: Double -> Integer -> Double
raizcuadrada a c = raizcuadradaprima a a c

pertenecealalistaono :: Integer -> [Integer] -> Bool
pertenecealalistaono a [] = False
pertenecealalistaono a b = if a == head(b) then True else pertenecealalistaono a (tail(b))

parimpar xs = [if mod x 2 == 1 then "impar" else "par"| x <- xs]

triangulos = [(a,b,sqrt(a^2+b^2))|a <-[1..10], b <- [1..10]]
triangulos' = [(a,b,c)|a <-[1..10], b <- [1..10], c <- [1..10], c^2 == a^2 + b^2]
triangulosrectangulos :: Integer -> [(Integer,Integer,Integer)]
triangulosrectangulos n = [(a,b,c)|a <-[1..n], b <- [1..n], c <- [1..n], c^2 == a^2 + b^2]

triangulosrectangulosconperimetro :: Integer -> Integer -> [(Integer,Integer,Integer)]
triangulosrectangulosconperimetro n j = [(a,b,c)|a <-[1..n], b <- [1..n], c <- [1..n], c^2 == a^2 + b^2, a+b+c == j]


dados = [(a,b,c)|a <-[1..6], b <-[1..6], c <-[1..6]]
probabilidad :: Integer -> Float
probabilidad n = (fromIntegral(length([(a,b,c)|a <-[1..6], b <-[1..6],c <-[1..6], a+b+c >= n])))/6^3
--ndados :: Integer -> [Integer]
--ndados n = 

--dados = 

probabilidad' = (fromIntegral(length([(a,b)|a <-[1..6], b <-[1..6], (fst(a,b) == 1 || fst(a,b) == 5) || (snd(a,b) == 1 || snd(a,b) == 5)])))/6^2
--calcula la probabilidad de que al tirar dos dados salga al menos un 1 o un 5

probabilidad'' :: Integer -> Float --ejercicio de proba clase practica 4, se tira 10 veces un dado, y se desea la probabilidad de que la suma sea mayor a 40
probabilidad'' n = (fromIntegral(length([(a,b,c,d,e,f,g,h,i,j)|a <-[1..6], b <-[1..6],c <-[1..6],d <-[1..6], e <-[1..6], f <-[1..6], g <-[1..6], h <-[1..6], i <-[1..6], j <-[1..6], a+b+c+d+e+f+g+h+i+j > n, not (p a b c d e f g h i j), not (q a b c d e f g h i j)])))/6^10
  where p a b c d e f g h i j = (a >= 4 && b >= 4 && c >= 4 && d >= 4 && e >= 4 && f >=4 && g >= 4 && h >= 4 && i >= 4 && j >= 4)
        q a b c d e f g h i j = (a <= 3 && b <= 3 && c <= 3 && d <= 3 && e <= 3 && f <= 3 && g <= 3 && h <= 3 && i <= 3 && j <= 3)
  
esCreciente :: (Ord a) => [a] -> Bool
esCreciente xs 
 | xs == [] = True
 | length(xs) == 1 = True
 | head(xs) < head(tail(xs)) = esCreciente (tail(xs))
 | otherwise = False

bubblesort' :: (Ord a) => [a] -> [a] -> [a]
bubblesort' xs js = if length xs == 1 || length xs == 0 then xs 
                    else (if not (esCreciente xs) then (if head(js) > head(tail(js)) then bubblesort (head(tail(js)): [head(js)] ++ tail(tail(js))) 
                    else head (js) : bubblesort (tail(js))) else xs)
 
bubblesort :: (Ord a) => [a] -> [a]
bubblesort xs = bubblesort'' xs xs

bubblesort'' :: (Ord a) => [a] -> [a] -> [a]
bubblesort'' xs js 
 | esCreciente xs = xs
 | otherwise = (if head(js) > head(tail(js)) then bubblesort'' xs (head(tail(js)): [head(js)] ++ tail(tail(js))) 
 else head (js) : bubblesort'' xs (tail(js)))
 
--sorting :: (Ord a) => [a] -> [a]
--sorting xs = [a|a <- xs, ]

errores = error "Nooooooo"

minimo' :: (Ord a) => [a] -> a --otra implementeacion
minimo' [] = error "lista vacia"
minimo' [x] = x
minimo' [x,y] = if x <= y then x else y
minimo' (x:y:ys) = if x <= y then minimo (x:ys) else minimo (y:ys) 

minimo :: (Ord a) => [a] -> a
minimo (x:y:ys)
 | length(x:y:ys) == 0 = error "lista vacia"
 | length(x:y:ys) == 1 = x
 | length(x:y:ys) == 2 = if x <= y then x else y 
 | otherwise = if x <= y then minimo (x:ys) else minimo (y:ys)
 
maximo :: (Ord a) => [a] -> a
maximo [] = error "lista vacia"
maximo [x] = x
maximo (x:xs)
 | x > head xs = maximo (x:tail xs)
 | otherwise = maximo xs

selection_sort :: (Ord a) => [a] -> [a] --complejidad n^2 para todo caso
selection_sort [] = []
selection_sort (x:[]) = [x]
selection_sort (x:xs) = minimum (x:xs) : selection_sort([b|b <- (x:xs), b /= minimum (x:xs)])  -- b /= minimum(x:xs) ordena sin dejar repetidos

--remover :: a -> [a] -> [a]
--remover a [a] 
 -- | not (a elem [a]) = [a]
 -- | otherwise = [a] !! n ++ 
 
posicion :: (Eq a) => a -> [a] -> Int -- devuelve la posicion de un elemento en la lista
posicion n a 
 | n == head a = 0 
 | otherwise = 1 + posicion n (tail a)

cortar_izq :: Int -> [a] -> [a] --corta la lista por la posicion n y se queda con la parte posterior
cortar_izq n xs 
 | n > (length xs) -1 = []
 | otherwise = (xs !! n) : cortar_izq(n+1) xs

cortar_der :: Int -> [a] -> [a]--corta por la derecha pero en forma invertida, ej cortar_der 5 [1..10] = [5,4,3,2,1]
cortar_der 0 xs = []
cortar_der 1 xs = [xs !! 0]
cortar_der n xs = xs !! (n-1) : cortar_der (n-1) xs

busqueda_lineal :: (Eq a) => a -> [a] -> Bool
busqueda_lineal a [] = False
busqueda_lineal a (x:xs) 
 | a == x = True
 | otherwise = busqueda_lineal a (xs)

busqueda_binaria :: (Ord a) => a -> [a] -> Bool --solo aplicable a listas ordenadas **usar takeWhile y drop en vez de cortar_der y cortar_izq
busqueda_binaria a [] = False
busqueda_binaria a (xs) 
 | a == xs !! (div (length xs) 2) = True
 | otherwise = if a >  xs !! (div (length xs) 2) 
               then busqueda_binaria a (cortar_izq ((div (length xs) 2)+1) xs) 
			   else busqueda_binaria a (reverse(cortar_der (div (length xs) 2) xs)) 

quicksort :: (Ord a) => [a] -> [a]
quicksort  []           =  []
quicksort (x:xs)        =  quicksort [y | y <- xs, y<x ]
                        ++ [x]
                        ++ quicksort [y | y <- xs, y>=x]

inversiones :: (Ord a) => [a] -> Int --busca los pares invertidos en una lista, ej inversiones [3,2,1] = |(3,2), (3,1), (2,1)| = 3
inversiones xs = length[(a,b)| a <-xs, b <-xs, a > b, (posicion a xs) < (posicion b xs)] --tiene complejidad O(n^2) y mejor caso tambien n^2

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

numerocombinatorio :: Integer -> Integer -> Integer
numerocombinatorio n r
 | n < r || r < 0 = 0
 | otherwise = div (factorial n) (factorial (n-r)* (factorial r))

factorialdelista :: [Integer] -> Integer
factorialdelista [] = 1
factorialdelista (x:xs) = factorial x * factorialdelista xs
 
coeficientemultinomial ::Integer -> [Integer] -> Integer
coeficientemultinomial n xs
 | sum(xs) /= n = error "la suma de la lista debe ser n"
 | otherwise = div (factorial n) (factorialdelista xs)  

--compareWithHundred :: (Read a, Ord a) => a -> Ordering  
--compareWithHundred = compare 'a' 

haymayusculaono :: String -> Bool
haymayusculaono "" = False
haymayusculaono (x:xs) 
	| elem x ['A'..'Z'] = True
	| otherwise = haymayusculaono (xs)

detectordeespacios :: String -> Int -- detecta en que lugar aparece el primer espacio en blanco en un string
detectordeespacios "" = error "no hay espacios"
detectordeespacios (x:xs)
	| x == ' ' = 0 
	| otherwise = 1 + detectordeespacios xs
	
	
palabrasutilizadas :: String -> Int --cuenta la cantidad de palabras, es necesario dejar un espacio en blanco al final
palabrasutilizadas "" = 0
palabrasutilizadas (x:xs)
	| elem x ['a'..'z'] || elem x ['A'..'Z'] = 1 + palabrasutilizadas ( (cortar_izq (detectordeespacios (x:xs)) (x:xs)) )
	| otherwise = palabrasutilizadas xs

funcion1 :: (Eq a) => ([a] -> a) -> [a] -> [a] -> Bool --funcion de orden alto, toma como parametro una funcion y 
funcion1 f a b = f a == f b -- 2 listas y devuelve Bool, segun f a == f b
							--el Eq es necesario porque se esta utilizando "=="

--Comentario: Supongamos que se desea crear dos funciones, f1 y f2, que tomen como parametros dos listas 
--y ambas aplican una funcion particular a sus otros parametros (las listas en este caso).
--En vez de hacer esto, se puede crear un funcion que tome a su vez como parametro una funcion y dos listas. 
--Luego, si se desea aplicar una funcion en particular a dichas listas, solo es necesario llamar (o evaluar) 
--la funcion con la funcion parametro que se desea utilizar, como en el caso de funcion1 (siempre que la funcion
--parametro pueda evaluarse con los otros parametros) 
 
funcion2 :: ([a] -> Int) -> [a] -> [a] -> Bool
funcion2 f a b = f a == f b --en este caso no es necesario (Eq a) =>... porque la funcion parametro devuelve
							--un Int, y es sabido que se puede realizar "==" con los Int (obvio)
--Aparte: si se quiere hacer raiz cubica, 2 ** (1/3) etc

distancia :: (Floating a) => (a,a) -> (a,a) -> a -- (**) y (^) tienen tipos distintos
distancia (a,b) (c,d) = ((a-c)**2 + (b-d)**2)**(1/2) --distancia entre 2 puntos 

distanciasdelosPares :: (Floating a, Ord a) => [(a,a)] -> [((a,a),(a,a),a)]--devuelve todas las distancias entre todos los puntos 
distanciasdelosPares [x] = [(x,x,0)]--los puntos enlazadas con los pares correspondientes a esa distancia
distanciasdelosPares (x:xs) = [(j,k,distancia j k)| j <- (x:xs), k <- (x:xs), posicion j (x:xs) < posicion k (x:xs)]

distanciaMasCorta :: (Floating a, Ord a) => [(a,a)] -> a--encuentra la distancia mas corta entre una lista de puntos
distanciaMasCorta (x:xs) = minimum[tercero a| a <- (distanciasdelosPares (x:xs))]

primero :: ((a,a),(a,a),a) -> (a,a)--utilizadas para obterner las distintas coordenadas de estas tuplas particulares
primero ((a,b),_,_) = (a,b)
segundo :: ((a,a),(a,a),a) -> (a,a)
segundo (_,(a,b),_) = (a,b) 
tercero :: ((a,a),(a,a),a) -> a
tercero (_,_,a) = a

parMasCercano :: (Floating a, Ord a) => [(a,a)] -> [((a,a),(a,a))] -- 4 horas me llevaste...
parMasCercano (x:xs) 
 | distanciaMasCorta (x:xs) == tercero (head (distanciasdelosPares (x:xs))) = [(primero (head (distanciasdelosPares (x:xs))) , segundo (head (distanciasdelosPares (x:xs))))]
 | otherwise = parMasCercano xs
--encuentra la tupla cuya 3ra coordenada (la distancia) coincide con la distancia mas corta y 
--devuelve los dos puntos que originaron dicha distancia

ramanujan :: Integer -> Integer -> [(Integer, Integer)]
ramanujan n m
 | n == m = ramanujanbis n
 | otherwise = if ramanujanbis n == [] then ramanujanbis (n+1) else ramanujanbis n

ramanujanbis :: Integer -> [(Integer, Integer)]
ramanujanbis n = [(n,a)| a <- [(div n 2)..n], sum [1..a] == sum [a..n]]      

ramanujantris :: Integer -> [(Integer, Integer, Integer)]
ramanujantris n = [(a,sum [1..(a-1)],sum [(a+1)..n])| a <- [1..n]]      

determinante2x2 :: [[Integer]] -> Integer --calcula determinante de una matriz 2x2
determinante2x2 [[a,b],[c,d]] = a*b - c*d

