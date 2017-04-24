-- --------------------------------------------- --
-- -----------| Haskell Problem #26 -----------| --
-- --------------------------------------------- --

-- (**) Generate the combinations of K objects chosen from N elements
-- Prelude > choose 3 '(a b c d e f)
-- ((A B C) (A B D) (A B E) ... )

choose				:: Int -> [a] -> [[a]]
choose 0 _			= [[]]
choose 1 xs			= [[i] | i <- xs]
choose _ []			= [[]]
choose n xs			= concat $ [map ((:) (xs !! i)) $ choose (n - 1) $ drop (i + 1) xs
						| i <- [0..(length xs) - 2]]