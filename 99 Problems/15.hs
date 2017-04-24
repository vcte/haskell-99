-- --------------------------------------------- --
-- -----------| Haskell Problem #15 -----------| --
-- --------------------------------------------- --

-- (**) Replicate the elements of a list n times
-- Prelude repl '(a b c) 3
-- (A A A B B B C C C)

repl			:: [a] -> Int -> [a]
repl xs	n		= foldr (++) [] $ map (replicate n) xs