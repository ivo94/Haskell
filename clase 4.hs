prodInterno :: [Float] -> [Float] -> Float
prodInterno a b 
 | length a == 1 && length b == 1 = head a * head b 
 | length a > 1 && length b > 1 = head a * head b  + prodInterno (tail a)(tail b)
 
prodInterno2 :: [Float] -> [Float] -> Float
prodInterno2 a b 
 | null a && null b = 0
 | otherwise = head a * head b  + prodInterno2 (tail a)(tail b)

 