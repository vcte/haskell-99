-- --------------------------------------------- --
-- -----------| Haskell Problem #3 |------------ --
-- --------------------------------------------- --

-- (*) Find the K'th element of a list. 1-index.
-- Prelude > kEle [1, 2, 3, 4] 3
-- 3

kEle			:: [a] -> Integer -> a
kEle (x:_) 1	= x
kEle (_:xs) n	= kEle xs (n - 1)