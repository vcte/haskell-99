-- --------------------------------------------- --
-- -----------| Haskell Problem #11 -----------| --
-- --------------------------------------------- --

-- (*) Modified run-length encoding of a list
-- Prelude > encode2 '(a a a a b c c a a d e e e e)
-- ((4 A) B (2 C) (2 A) D (4 E))

pack				:: (Eq a, Num a) => [a] -> [[a]]
pack []				= [[]]
pack [x]			= [[x]]
pack (x:xs)			= if x == (head $ pack xs) !! 0
					  then ((:) x $ head $ pack xs) : (tail $ pack xs)
					  else (:) [x] $ pack xs

encode				:: (Eq a, Num a) => [a] -> [[a]]
encode x			=  map (\x -> (length x, head x)) $ pack x

encode2				:: (Eq a, Num a) => [a] -> [[a]]
encode2 x			= map f $ encode x
						where 
							  f (1, x) = (x)
							  f (n, x) = (n, x)