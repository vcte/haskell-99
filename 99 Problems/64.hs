-- --------------------------------------------- --
-- -----------| Haskell Problem #64 -----------| --
-- --------------------------------------------- --

-- (**) Tree layout - x(v) is position in ordered sequence, y(v) is depth
-- layout tree64
-- [(Branch 'n', 8, 1), (Branch 'k', 6, 2), (Branch 'c', 2, 3)]

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
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

dispLayout					:: [(Char, Int, Int)] -> IO ()
dispLayout xs				= putStrLn $ dispLayout' xs 1
							where
								blank = take 30 $ concat $ repeat " "
								dispLayout' [] _ = ""
								dispLayout' xs n = let xs2 = filter (\(a, b, c) -> c == n) xs;
													   xs3 = filter (\x -> not $ x `elem` xs2) xs in
									dispLayout'' xs2 blank ++ "\n" ++ dispLayout' xs3 (n + 1)
								dispLayout'' [] b = b
								dispLayout'' (x:xs) b = let (x1, x2, x3) = x in
									dispLayout'' xs $ insert b x1 x2
								insert b x1 x2 = let (b1, b2) = splitAt x2 b in
									init b1 ++ (x1 : b2)

layout						:: Tree a -> [(a, Int, Int)]
layout a					= acc a 1 []
							where
								acc Empty _ xs = xs
								acc (Branch n l r) d xs = 
									let xs2 = acc l (d + 1) xs
									in acc r (d + 1) $ xs2 ++ [(n, (length xs2) + 1, d)]