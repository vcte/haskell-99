-- --------------------------------------------- --
-- -----------| Haskell Problem #13 -----------| --
-- --------------------------------------------- --

-- (**) Direct run-length encoding of a list
-- Prelude encode3 '(a a a a b c c a a d e e e e)
-- ((4 A) B (2 C) (2 A) D (4 E))

encode3				:: (Eq a, Num a) => [a] -> [[a]]
encode3 x			= map f . g x
						where
							f 			= \x -> (length x, head x)
							g [x] 		= [[x]]
							g (x:xs) 	= if x == (head $ g xs) !! 0
										then ((:) x $ head $ g xs) : (tail $ g xs)
										else (:) [x] $ g xs