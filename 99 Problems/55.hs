-- --------------------------------------------- --
-- -----------| Haskell Problem #55 -----------| --
-- --------------------------------------------- --

-- (**) Construct completely balanced binary trees
-- Prelude > balTree 4
-- Branch 'x' (Branch 'x' Empty Empty) 
--            (Branch 'x' Empty 
--                        (Branch 'x' Empty Empty)),
-- Branch 'x' (Branch 'x' Empty Empty) 
--            (Branch 'x' (Branch 'x' Empty Empty) 
--                        Empty),
-- Branch 'x' (Branch 'x' Empty 
--                        (Branch 'x' Empty Empty)) 
--            (Branch 'x' Empty Empty),
-- Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) 
--                        Empty) 
--            (Branch 'x' Empty Empty)

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

balTree						:: Int -> [Tree Int]
balTree 0					= [Empty]
balTree 1					= [Branch 0 Empty Empty]
balTree n					| n `mod` 2 == 0 = [Branch 0 tx ty
												| (x, y) <- [(n `div` 2, (n `div` 2) - 1), ((n `div` 2) - 1, n `div` 2)]
												, tx <- balTree x, ty <- balTree y]
							| otherwise      = [Branch 0 tx ty
												| tx <- balTree ((n - 1) `div` 2)
												, ty <- balTree ((n - 1) `div` 2)]