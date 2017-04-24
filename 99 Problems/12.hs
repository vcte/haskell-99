-- --------------------------------------------- --
-- -----------| Haskell Problem #12 -----------| --
-- --------------------------------------------- --

-- (**) Decode a run-length encoding of a list
-- Prelude > decode '((4 A) B (2 C) (2 A) D (4 E))
-- (a a a a b c c a a d e e e e)

decode				:: [ListItem a] -> [a]
decode x			= foldr (++) $ map f x
						where
							f (Single e)		= [e]
							f (Multiple n e)	= replicate n e