-- --------------------------------------------- --
-- -----------| Haskell Problem #71 -----------| --
-- --------------------------------------------- --

-- (*) Determine the internal path length of a tree
-- ipl tree5
-- 9

data Tree a = Node a [Tree a]
              deriving (Show, Eq)

tree1 = Node 'a' []
 
tree2 = Node 'a' [Node 'b' []]
 
tree3 = Node 'a' [Node 'b' [Node 'c' []]]
 
tree4 = Node 'b' [Node 'd' [], Node 'e' []]
 
tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

showNodes					:: (Show a) => [Tree a] -> String
showNodes []				= ""
showNodes [Node a xs]		= "Node " ++ show a ++ " [" ++ showNodes xs ++ "]"
showNodes (Node a xs : ys)	= "Node " ++ show a ++ " [" ++ showNodes xs ++ "], " ++ showNodes ys
				
showTree					:: (Show a) => Tree a -> String
showTree a					= showNodes [a]	
					  
{-dispTree					:: (Show a, Eq a) => Tree a -> IO ()
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
								dispTree'' x  = putStrLn $ concat $ map (\s -> s ++ "\n") $ map concat x-}

nodesTree					:: Tree a -> Int
nodesTree a					= sizeNodes [a]
							where
								sizeNodes []			   = 0
								sizeNodes [Node a xs]	   = 1 + sizeNodes xs
								sizeNodes (Node a xs : ys) = 1 + sizeNodes xs + sizeNodes ys

depthTree					:: Tree a -> Int
depthTree a					= depthNodes [a]
							where
								depthNodes []			    = 0
								depthNodes [Node a xs]	    = 1 + depthNodes xs
								depthNodes (Node a xs : ys) = max (1 + depthNodes xs) (depthNodes ys)
								
strToTree					:: String -> Tree Char
strToTree []				= Node ' ' []
strToTree (x:'^':"")		= Node  x  []
strToTree (x:xs)			= Node  x  ys
							where 
								z = map fst $ filter ((==) 0 . snd) $ zip [0..] $ 
									scanl (+) 0 $ map conv $ xs
								ys = map strToTree $ map (uncurry (sub xs)) $ zip (init z) (tail z)
								conv '^' = -1
								conv  _  = 1
								sub s a b = take (b - a) $ drop a s

ipl							:: Tree a -> Int
ipl a						= ipl' 0 a
							where
								ipl' _ (Node a []) = 0
								ipl' n (Node a xs) = ((n + 1) * (length xs)) + (sum $ map (ipl' (n + 1)) xs)