-- --------------------------------------------- --
-- -----------| Haskell Problem #14 -----------| --
-- --------------------------------------------- --

-- (*) Duplicate the elements of a list
-- Prelude dup '(a b c c d)
-- (A A B B C C C C D D)

dup				:: [a] -> [a]
dup	xs			= foldr (++) [] $ map (\x -> [x, x]) xs