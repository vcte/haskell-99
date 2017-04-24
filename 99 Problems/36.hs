-- --------------------------------------------- --
-- -----------| Haskell Problem #36 -----------| --
-- --------------------------------------------- --

-- (**) Determine the prime factors of a positive integer
-- Prelude > factor' 315
-- ((3, 2), (5, 1), (7, 1))

prime					:: Int -> Bool
prime x					= all (/= 0) $ map (x `mod`) [2..(x-1)]

coprime					:: Int -> Int -> Bool
coprime	x y				= 1 == gcd x y

factor					:: Int -> [Int]
factor 1				= []
factor x				= p : (factor n)
						where
							p = head $ filter (not . coprime x) primes
							primes = filter prime [2..]
							n = div x p
							
factor'					:: Int -> [(Int, Int)]
factor' x				= f $ factor x
						where
							f = \xs -> if null xs then [] else
										(head xs, 
										length $ takeWhile ((==) $ head xs) xs)
										: (f $ dropWhile ((==) $ head xs) xs)