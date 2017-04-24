-- --------------------------------------------- --
-- -----------| Haskell Problem #2 |------------ --
-- --------------------------------------------- --

-- (*) Find the last but one element of a list
-- Prelude > last1 [1, 2, 3, 4]
-- 3

islast			:: [a] -> Bool
islast [x]		= True
islast (xs)		= False

last1			:: [a] -> a
last1 [x]		= x
last1 (x:xs)	= case islast xs of
					True -> x
					False -> last1 xs