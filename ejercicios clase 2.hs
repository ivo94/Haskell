crearPar :: a-> b -> (a,b)
crearPar x y = (x,y)
invertir :: (a,b) -> (b,a)
invertir (a,b) = (b,a)
distancia :: (Float,Float) -> (Float,Float) -> Float
distancia (a,b) (c,d) = sqrt((c-a)^2+(d-b)^2)
f n 
 | n > 0 = sqrt (n)
 | n == 0 = 1
pendiente :: (Float,Float) -> (Float,Float) -> Float
pendiente (a,b) (c,d) = ((d-b)/(c-a))