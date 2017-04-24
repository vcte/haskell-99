-- --------------------------------------------- --
-- -----------| Haskell Problem #33 -----------| --
-- --------------------------------------------- --

-- (*) Determine whether two positive integers are coprime
-- Prelude > coprime 35 64
-- True

coprime					:: Int -> Int -> Bool
coprime	x y				= 1 == gcd x y