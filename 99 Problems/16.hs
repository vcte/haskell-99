-- --------------------------------------------- --
-- -----------| Haskell Problem #16 -----------| --
-- --------------------------------------------- --

-- (**) Drop every N'th element from a list
-- Prelude dropn "abcdefghik" 3
-- "abdeghk"

dropn				:: [a] -> Int -> [a]
dropn [] _			= []
dropn xs n			= (take (n - 1) xs) ++ (dropn (drop n xs) n)