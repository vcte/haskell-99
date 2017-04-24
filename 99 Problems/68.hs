-- --------------------------------------------- --
-- -----------| Haskell Problem #68 -----------| --
-- --------------------------------------------- --

-- (**) Preorder and inorder sequences of binary trees
-- let { Just t = stringToTree "a(b(d,e),c(,f(g,)))" ;
--           po = preorder t ;
--           io = inorder t } in preInTree po io >>= print
-- Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) 
-- (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty))

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

preorder					:: (Show a) => Tree a -> String
preorder (Empty)			= ""
preorder (Branch a l r)		= show a ++ preorder l ++ preorder r

inorder						:: (Show a) => Tree a -> String
inorder (Empty)				= ""
inorder (Branch a l r)		= inorder l ++ show a ++ inorder r

preInTree					:: String -> String -> Tree String
preInTree [] []				= Empty
preInTree [p] [i]			= Branch [p] Empty Empty
preInTree (p:ps) io			= Branch [p] (preInTree ps2 is2) (preInTree ps3 is3)
							where
								(is2, i:is3) = break ((==) p) io
								(ps2, ps3)   = splitAt (length is2) ps