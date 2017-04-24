-- --------------------------------------------- --
-- -----------| Haskell Problem #32 -----------| --
-- --------------------------------------------- --

-- (**) Determine the greatest common divisor of two positive integers
-- Prelude > gcd 36 63
-- 9

gcd'					:: Int -> Int -> Int
gcd' m 0				= abs m
gcd' m n				= gcd' n (m `mod` n)