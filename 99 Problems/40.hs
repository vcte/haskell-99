-- --------------------------------------------- --
-- -----------| Haskell Problem #40 -----------| --
-- --------------------------------------------- --

-- (**) Goldbach's conjecture
-- Prelude > goldbach 28
-- (5, 23)

prime					:: Int -> Bool
prime x					= all (/= 0) $ map (x `mod`) [2..(x-1)]

primes					:: Int -> Int -> [Int]
primes x y				= filter prime [x..y]

goldbach				:: Int -> (Int, Int)
goldbach x				= head [(m, n) | m <- primes 0 x,
									n <- primes 0 x,
									m + n == x]