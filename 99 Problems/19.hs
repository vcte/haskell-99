-- --------------------------------------------- --
-- -----------| Haskell Problem #19 -----------| --
-- --------------------------------------------- --

-- (**) Rotate a list N places to the left
-- Prelude > (rotate '(a b c d e f g h) 3)
-- (D E F G H A B C)

rotate 			:: [a] -> Int -> [a]
rotate xs n		= take l $ drop (n `mod` l) $ cycle xs
					where l = length xs