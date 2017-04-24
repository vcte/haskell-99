-- --------------------------------------------- --
-- -----------| Haskell Problem #67 -----------| --
-- --------------------------------------------- --

-- (*) String representations of binary trees
-- stringToTree "x(y,a(,b))"
--  Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )

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

treeToString						:: (Show a) => Tree a -> String
treeToString (Empty)				= ""
treeToString (Branch a Empty Empty)	= show a
treeToString (Branch a l r)			= show a ++ "(" ++ treeToString l ++ "," ++ treeToString r ++ ")"

-- no spaces
stringToTree						:: String -> Tree String
stringToTree []						= Empty
stringToTree [x]					= Branch [x] Empty Empty
stringToTree (a:'(':b:',':c:')':e)	= Branch [a] (Branch [b] Empty Empty) (Branch [c] Empty Empty)
stringToTree x						= Branch (take (i1 - 1) x) (stringToTree $ sub x (i1 + 1) (i3 + 1))
															   (stringToTree $ sub x (i3 + 2) (i5 + 1))
									where
										lst = tail $ scanl (+) 0 $ map conv x
										i1 = 1 + (length $ takeWhile ((>) 1) lst)
										i2 = 1 + (length $ takeWhile ((>) 2) lst)
										i3 = 1 + i2 + (length $ takeWhile ((/=) 1) $ drop i2 lst)
										i4 = 1 + i3 + (length $ takeWhile ((>) 2) $ drop i3 lst)
										i5 = 1 + i4 + (length $ takeWhile ((/=) 1) $ drop i4 lst)
										sub = \s a b -> take (b - a) $ drop (a - 1) s
										conv a = case a of 
											'(' -> 1
											')' -> -1
											otherwise -> 0