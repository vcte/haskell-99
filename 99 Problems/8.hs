-- --------------------------------------------- --
-- -----------| Haskell Problem #8 |------------ --
-- --------------------------------------------- --

-- (**) Eliminate consecutive duplicates of list elements
-- Prelude > compress '(a a a a b c c a a d e e e e)
-- (A B C A D E)

compress			:: (Eq a) => [a] -> [a]
compress []			= []
compress [x]		= [x]
compress (x:y:xs)	= case x == y of
					True -> compress $ x : xs
					False -> (:) x $ compress $ y : xs