-- --------------------------------------------- --
-- -----------| Haskell Problem #7 |------------ --
-- --------------------------------------------- --

-- (**) Flatten a nested list structure
-- Prelude > flat '(a (b (c d) e))
-- (A B C D E)

data Nest a = Int a | List [Nest a]

flat			:: Nest a -> [a]
flat (Int x)	= [x]
flat (List x)	= foldr (++) [] $ map flat x