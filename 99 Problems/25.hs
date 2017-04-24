-- --------------------------------------------- --
-- -----------| Haskell Problem #25 -----------| --
-- --------------------------------------------- --

-- (*) Generate a random permutation of a list
-- Prelude > randperm (a b c d e f)
-- (B A D C E F)

import System.Random

lotto					:: (Eq a) => [a] -> IO [a]
lotto []				= return []
lotto xs				= fmap (\x -> (perm xs) !! x) $ getStdRandom $ randomR (0, (foldr1 (*) [1 .. length xs]) - 1)
						where
							perm a = foldr (\x xs -> 
												[i : s | i <- a, s <- xs, i `notElem` s])
												[[]] [1..length a]