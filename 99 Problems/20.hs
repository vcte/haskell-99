-- --------------------------------------------- --
-- -----------| Haskell Problem #20 -----------| --
-- --------------------------------------------- --

-- (*) Remove the K'th element from a list
-- Prelude > remove '(a b c d) 2
-- (B (A C D))

remove				:: [a] -> Int -> (a, [a])
remove xs n			= (head $ drop (n - 1) xs,
					   take (n - 1) xs ++ drop n xs)