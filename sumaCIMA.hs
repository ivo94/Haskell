sumaCIMA :: Integer -> Integer 
sumaCIMA n = sum (filter (p)(takeWhile (<n) [1,3..]))
 where p x = x^2 < n 