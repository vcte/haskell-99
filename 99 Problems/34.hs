-- --------------------------------------------- --
-- -----------| Haskell Problem #34 -----------| --
-- --------------------------------------------- --

-- (**) Calculate Euler's totient function phi(m)
-- Prelude > phi 10
-- 4

coprime					:: Int -> Int -> Bool
coprime	x y				= 1 == gcd x y

phi 					:: Int -> Int
phi m					= length $ filter (coprime m) [1..(m - 1)]