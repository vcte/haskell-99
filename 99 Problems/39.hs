-- --------------------------------------------- --
-- -----------| Haskell Problem #39 -----------| --
-- --------------------------------------------- --

-- (*) A list of prime numbers
-- Prelude > primesR 10 20
-- (11, 13, 17, 19)

prime					:: Int -> Bool
prime x					= all (/= 0) $ map (x `mod`) [2..(x-1)]

primesR					:: Int -> Int -> [Int]
primesR x y				= filter prime [x..y]