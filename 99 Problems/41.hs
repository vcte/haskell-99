-- --------------------------------------------- --
-- -----------| Haskell Problem #41 -----------| --
-- --------------------------------------------- --

-- (**) Goldbach's conjecture - range
-- Prelude > goldbachList 9 20
-- ((3, 7), (5, 7), (3, 11), (3, 13), (5, 13), (3, 17))

prime					:: Int -> Bool
prime x					= all (/= 0) $ map (x `mod`) [2..(x-1)]

primes					:: Int -> Int -> [Int]
primes x y				= filter prime [x..y]

goldbach				:: Int -> (Int, Int)
goldbach x				= head [(m, n) | m <- primes 2 x,
									n <- primes 2 x,
									m + n == x]
									
goldbachList			:: Int -> Int -> [(Int, Int)]
goldbachList x y		= map goldbach $ filter (\x -> 0 == mod x 2) [x..y]