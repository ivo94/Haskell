import Data.Char
restosmodulo2 :: Integer -> [Integer]
restosmodulo2 n 
 | n == 0 = 0 : []
 | n == 1 = 1 : []
 | n /= 1 = (mod n 2) : (restosmodulo2 (div n 2))

reverso :: [a] -> [a] 
reverso [] = []
reverso xs = (reverso(tail xs)) ++ [head xs]

conversorbinario :: Integer -> Integer
conversorbinario n = borradordecomillas (borradordecomas(reverse(restosmodulo2 n)))

coma :: String -> String
coma a
 | length a == 0 = ""
 | isNumber (head a) == True = a
 | isNumber (head a) == False = "" ++ coma (tail a)
 
esNatural :: Float -> Bool
esNatural n = elem n [1..]

restosmodulon :: Integer -> Integer -> [Integer]
restosmodulon n b
 | n == 0 = []
 | n == 1 = 1 : []
 | n /= 1 = (mod n b) : (restosmodulon (div n b) b)
 
conversorAsistemadeNumeracion :: Integer -> Integer -> String
conversorAsistemadeNumeracion n b = borradordecomas (reverse (restosmodulon n b))

borradordecomas :: (Show a) => [a] -> String --esaaaaaa
borradordecomas [] = ""
borradordecomas a = show(head a) ++ borradordecomas (tail a)

borradordecomillas :: String -> Integer --ohhhhhhhhh
borradordecomillas a = read a :: Integer --los :: indican como se lee el string, en este caso como un Integer






