-- --------------------------------------------- --
-- -----------| Haskell Problem #17 -----------| --
-- --------------------------------------------- --

-- (*) Split a list into two parts
-- Prelude > split '(a b c d e f g h i k) 3
-- ((A B C) (D E F G H I K))

split				:: [a] -> Int -> ([a], [a])
split xs n			= ((take n xs), (drop n xs))

-- split'				:: [a] -> Int -> ([a], [a])
-- split' xs n			= (filter f1 x2, filter f2 x2) <- tuple
-- 						where
-- 							x2 = zip xs [1..length xs]
-- 							f1 = \x -> snd x <= n
-- 							f2 = \x -> snd x > n