-- --------------------------------------------- --
-- -----------| Haskell Problem #35 -----------| --
-- --------------------------------------------- --

-- (**) Determine the prime factors of a positive integer
-- Prelude > factor 315
-- (3, 3, 5, 7)

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