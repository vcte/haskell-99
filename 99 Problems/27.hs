-- --------------------------------------------- --
-- -----------| Haskell Problem #27 -----------| --
-- --------------------------------------------- --

-- (**) Group the elements of a set into disjoint subsets
-- Prelude > group '(a b c d e f)
-- ( ( (A B C) (D E) (F) ) )

group 				:: [a] -> [[[a]]]
group []			= [[[]]]
group [x]			= [[[x]]]
group (x:xs)		= add ++ join
					where 
						add = map ((:) [x]) part
						join =  concat [[
							(take j (part !! i)) ++ ((x : ((part !! i) !! j)) : (drop (j + 1) (part !! i)))
							| j <- [0..(length (part !! i)) - 1]]
							| i <- [0..(length part) - 1]]
						part = group xs
						