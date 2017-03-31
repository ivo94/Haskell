esPositivo x
 | x > 0 = True
 | otherwise = False
esPar x
 | mod x 2 == 1 = False
 | mod x 2 == 0 = True
esPrimo x
 | x == 1 || x == (-1) = False
 | mod x 2 == 0 && x /= 2 = False
 | mod x 3 == 0 && x /= 3 = False
 | mod x 5 == 0 && x /= 5 = False
 | mod x 7 == 0 && x /= 7 = False
 | otherwise = True
 isPrime::Integer -> Bool
isPrime n 
    | n <= 3 = n > 1
	| n `mod` 2 == 0 || n `mod` 3 == 0 = False
	| otherwise = [] == (take 1 $ Data. List. filter (\x -> n `mod` x == 0 || n `mod` (x+2) == 0 ) 
                      $ takeWhile (\x -> x*x <= n) [5,7..])