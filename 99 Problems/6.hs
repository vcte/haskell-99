-- --------------------------------------------- --
-- -----------| Haskell Problem #6 |------------ --
-- --------------------------------------------- --

-- (*) Find whether a list is a palindrome
-- Prelude > palin [1, 2, 4, 8, 16, 8, 4, 2, 1]
-- True

palin			:: (Eq a) => [a] -> Bool
palin []		= True
palin [x]		= True
palin (x:xs)	= case x == last xs of
				 True -> palin $ tail xs
				 False -> False