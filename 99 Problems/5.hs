-- --------------------------------------------- --
-- -----------| Haskell Problem #5 |------------ --
-- --------------------------------------------- --

-- (*) Reverse a list
-- Prelude > rev [1, 2, 3, 4]
-- [4, 3, 2, 1]

rev				:: [a] -> [a]
rev []			= []
rev [x]			= [x]
rev (x:xs)		= rev xs ++ [x]