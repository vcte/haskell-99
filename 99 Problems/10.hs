-- --------------------------------------------- --
-- -----------| Haskell Problem #10 -----------| --
-- --------------------------------------------- --

-- (*) Run-length encoding of a list
-- Prelude > encode '(a a a a b c c a a d e e e e)
-- ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

pack				:: (Eq a) => [a] -> [[a]]
pack []				= [[]]
pack [x]			= [[x]]
pack (x:xs)			= if x == (head $ pack xs) !! 0
					  then ((:) x $ head $ pack xs) : (tail $ pack xs)
					  else (:) [x] $ pack xs

encode				:: (Eq a) => [a] -> [[a]]
encode x			=  map (\x -> (length x, head x)) pack x