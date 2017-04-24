-- --------------------------------------------- --
-- -----------| Haskell Problem #4 |------------ --
-- --------------------------------------------- --

-- (*) Find the number of elements in a list
-- Prelude > len [1, 2, 3, 4]
-- 4

len				:: [a] -> Integer
len []			= 0
len (x:xs)		= 1 + len xs