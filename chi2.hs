chi2 :: [Float] -> [Float] -> Float
chi2 a b 
 | length a > 0 && length b > 0 = (head a - head b)^2/(head b) + chi2 (tail a) (tail b)
 | otherwise = 0 

sumatoria :: Integer -> Integer
sumatoria a
 | a==0 = 0
 | otherwise = a + sumatoria(a-1)