-- --------------------------------------------- --
-- -----------| Haskell Problem #18 -----------| --
-- --------------------------------------------- --

-- (**) Extract a slice from a list
-- Prelude > slice '(a b c d e f g h i k) 3 7
-- (C D E F G)

slice				:: [a] -> Int -> Int -> [a]
slice xs l h		= [fst x | x <- x2, snd x >= l, snd x <= h]
						where
							x2 = zip xs [1..length xs]

slice'				:: [a] -> Int -> Int -> [a]
slice' xs l h		= take (h - l + 1) $ drop (l - 1) xs