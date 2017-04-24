-- --------------------------------------------- --
-- -----------| Haskell Problem #61 -----------| --
-- --------------------------------------------- --

-- (*) Count the leaves of a binary tree
-- leavesTree tree4
-- 2

-- (*) Collect the leaves of a binary tree
-- leaves tree4
-- [4, 2]

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

showTree					:: (Show a) => Tree a -> String
showTree (Empty) 			= "Empty"
showTree (Branch a l r)		= "Branch " ++ show a ++ " (" ++
							  show l ++ ") (" ++ show r ++ ")"
							  
dispTree					:: (Show a, Eq a) => Tree a -> IO ()
dispTree a					= dispTree'' $ dispTree' a 10 0 grid
							where
								grid = take (2 * (depthTree a)) $ repeat (take 30 $ repeat " ")
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
depthTree (Branch a l r)	= 1 + max (depthTree l) (depthTree r)

nodesTree					:: Tree a -> Int
nodesTree (Empty)			= 0
nodesTree (Branch a l r)	= 1 + nodesTree l + nodesTree r

leavesTree							:: Tree a -> Int
leavesTree (Empty)					= 0
leavesTree (Branch a Empty Empty)	= 1
leavesTree (Branch a l r)			= leavesTree l + leavesTree r

leaves							:: Tree a -> [a]
leaves (Empty)					= []
leaves (Branch x Empty Empty)	= [x]
leaves (Branch a l r)			= leaves l ++ leaves r