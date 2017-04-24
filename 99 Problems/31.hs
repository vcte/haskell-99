-- --------------------------------------------- --
-- -----------| Haskell Problem #31 -----------| --
-- --------------------------------------------- --

-- (**) Determine whether an integer is prime
-- Prelude > prime 7
-- True

prime				:: Int -> Bool
prime x				= all (/= 0) $ map (x `mod`) [2..(x-1)]