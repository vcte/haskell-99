-- --------------------------------------------- --
-- -----------| Haskell Problem #9 |------------ --
-- --------------------------------------------- --

-- (**) Pack consecutive duplicates of list elements into sublists
-- Prelude > pack '(a a a a b c c a a d e e e e)
-- ((A A A A) (B) (C C) (A A) (D) (E E E E))

pack				:: (Eq a) => [a] -> [[a]]
pack []				= [[]]
pack [x]			= [[x]]
pack (x:xs)			= if x == (head $ pack xs) !! 0
					  then ((:) x $ head $ pack xs) : (tail $ pack xs)
					  else (:) [x] $ pack xs