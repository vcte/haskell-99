-- --------------------------------------------- --
-- -----------| Haskell Problem #22 -----------| --
-- --------------------------------------------- --

-- (*) Create a list containing all integers within a given range
-- Prelude > range 4 9
-- (4 5 6 7 8 9)

range				:: Int -> Int -> [Int]
range x y 			= if (x == y)
					  then [x]
					  else [x] ++ range (x + 1) y