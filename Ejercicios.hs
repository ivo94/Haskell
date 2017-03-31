--Alumno: Ivo Begonja
--Turno: Viernes

--Clase 1
esPitagorica :: Float -> Float -> Float -> Bool
esPitagorica a b c 
 | a^2 + b^2 == c^2 = True
 | otherwise = False
 
--Clase 2
pendiente :: (Float,Float) -> (Float,Float) -> Float
pendiente (a,b) (c,d) = ((d-b)/(c-a))

--Clase 3
imparesMenores :: Integer -> Integer -> [Integer]
imparesMenores 0 m = []
imparesMenores n 1 = []
imparesMenores n m | mod (n-1) 2 == 0 = imparesMenores (n-1) m 
                   | mod (n-1) 2 == 1 = (n-1) : imparesMenores (n-2) m 

imparesMenoresA :: Integer -> [Integer]
imparesMenoresA n = imparesMenores n n

cuadradosMenores :: Integer -> Integer -> [Integer]
cuadradosMenores n m
 | (head (imparesMenoresA n))^2 > m = cuadradosMenores (head (imparesMenoresA n)) m
 | (head (imparesMenoresA n))^2 < m = head (imparesMenoresA n) : tail (imparesMenoresA n)
 
sumaImparesCuadradosMenoresA :: Integer -> Integer
sumaImparesCuadradosMenoresA 0 = 0 
sumaImparesCuadradosMenoresA n = sum (cuadradosMenores n n)

--Clase 4
prodInterno :: [Float] -> [Float] -> Float
prodInterno a b 
 | length a == 1 && length b == 1 = head a * head b 
 | length a > 1 && length b > 1 = head a * head b  + prodInterno (tail a)(tail b)
 
--Clase 5
diferenciaPositiva :: Float -> Float -> Bool
diferenciaPositiva a b 
 | a-b >= 0 = True
 | otherwise = False

esDecreciente :: [Float] -> Bool
esDecreciente [_] = True
esDecreciente [] = True
esDecreciente a | diferenciaPositiva (head a)  (head (tail a)) == True = esDecreciente (tail a)
                | diferenciaPositiva (head a)  (head (tail a)) == False = False
				
esCreciente :: [Float] -> Bool
esCreciente [_] = True
esCreciente [] = True
esCreciente a | diferenciaPositiva (head (tail a)) (head a) == True = esCreciente (tail a)
              | diferenciaPositiva (head (tail a)) (head a) == False = False

esOrdenada :: [Float] -> Bool
esOrdenada a 
 | esCreciente a == True || esDecreciente a == True = True
 | otherwise = False


--Clase 7 
divParcial :: Integer -> Integer -> [Integer]
divParcial n 1 = [1]
divParcial n m 
 | mod n m == 0 = m : divParcial n (m-1)
 | otherwise = divParcial n (m-1)
 
divisores :: Integer -> [Integer]
divisores n = divParcial n n

reverso :: [a] -> [a] 
reverso [] = []
reverso xs = (reverso(tail xs)) ++ [head xs]
 
divisoresOrdenados :: Integer -> [Integer]
divisoresOrdenados n = reverso (divisores n)

primerDivisor :: Integer -> Integer
primerDivisor 1 = 1
primerDivisor n = head (tail (divisoresOrdenados n))

meter :: Integer -> [Integer] 
meter n = primerDivisor n : []

factorizar :: Integer -> [Integer]
factorizar 1 = []
factorizar n = meter n ++ factorizar (div n (primerDivisor n))

 
--Clase 8
data Lista a = Vacia | Agregar a (Lista a)
iguales :: Eq a => Lista a -> Lista a -> Bool
iguales Vacia Vacia = True
iguales (Agregar a (b)) (Agregar c (d)) | a/= c = False
                                        | a==c = iguales (b) (d)

--Clase 9
numeroscon6div :: Integer -> [Integer]
numeroscon6div n = filter (p) [1..n]
 where p n = length (divisores n) == 6

numeroscon6divHasta1000 = numeroscon6div 1000