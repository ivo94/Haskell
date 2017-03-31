auxi2 :: Char -> [Char] -> Bool
auxi2 x y | length y == 0 =False
          | x==head y = True
          | x/=head y = auxi2 x (tail y)

auxi :: Char -> Bool		  
auxi x = auxi2 x ['a'..'z']

esMin :: Char -> Bool
esMin x = auxi x 

letANat :: Char -> Integer		  
letANat a = agarra a (zip ['a','b'..'z'] [0,1..25])

agarra :: Char -> [(Char,Integer)] -> Integer
agarra a b | a==fst(head b) = snd (head b)
           | a/=fst(head b) = agarra a (tail b)
		
natALet :: Integer -> Char
natALet a = agarra2 a (zip [0,1..25] ['a','b'..'z'])

agarra2 :: Integer -> [(Integer,Char)] -> Char
agarra2 a b 
 | a == fst (head b) = snd (head b)
 | a /= fst (head b) = agarra2 a (tail b) 
 
reversoletANat :: Char -> Integer
reversoletANat x = agarra x (zip ['a','b'..'z'] [25,24..0])

reversonatALet :: Integer -> Char
reversonatALet x = agarra2 x (zip [0,1..25] ['z','y'..'a'])

