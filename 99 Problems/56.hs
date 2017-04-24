-- --------------------------------------------- --
-- -----------| Haskell Problem #56 -----------| --
-- --------------------------------------------- --

-- (**) Symmetric binary trees
-- Prelude > symmetric (Branch 1 (Branch 2 Empty Empty) (Branch 3 Empty Empty))
-- True

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

showTree					:: (Show a) => Tree a -> String
showTree (Empty) 			= "Empty"
showTree (Branch a l r)		= "Branch " ++ show a ++ " (" ++
							  show l ++ ") (" ++ show r ++ ")"
							  
dispTree					:: (Show a, Eq a) => Tree a -> IO ()
dispTree a					= dispTree'' $ dispTree' a 4 0 grid
							where
								grid = take (2 * (depthTree a)) $ repeat (take (4 * (sizeTree a)) $ repeat " ")
								updateGrid a x y g = let (g1, g2:g3) = splitAt y g;
														 (g21, g22:g23) = splitAt x g2
													 in g1 ++ [g21 ++ (a : g23)] ++ g3
								dispTree' (Empty) _ _ g = g
								dispTree' (Branch a l r) x y g = 
									dispTree' r (x + 2) (y + 2) $ dispTree' l (x - 2) (y + 2) $ 
									(\g -> if (r /= Empty) then (updateGrid ">" (x + 1) (y + 1) g) else g) $
									(\g -> if (l /= Empty) then (updateGrid "<" (x - 1) (y + 1) g) else g) $
									updateGrid (show a) x y g
								dispTree'' x  = putStrLn $ concat $ map (\s -> s ++ "\n") $ map concat x

sizeTree					:: Tree a -> Int
sizeTree (Empty)			= 0
sizeTree (Branch a l r)		= 1 + sizeTree l + sizeTree r

depthTree					:: Tree a -> Int
depthTree (Empty)			= 0
depthTree (Branch a l r)	= 1 + max (sizeTree l) (sizeTree r)

symmetric 					:: Tree a -> Bool
symmetric (Empty)			= True
symmetric (Branch a l r)	= mirror l r
							where
								mirror (Empty) (Empty) = True
								mirror (Empty) (Branch a l r) = False
								mirror (Branch a l r) (Empty) = False
								mirror (Branch a l r) (Branch a' l' r') = 
									mirror l r' && mirror l' r