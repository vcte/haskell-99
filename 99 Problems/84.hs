-- --------------------------------------------- --
-- -----------| Haskell Problem #84 -----------| --
-- --------------------------------------------- --

-- (**) Construct the minimal spanning tree
-- prim Graph [1, 2, 3, 4, 5]
--            [(1,2,12),(1,3,34),(1,5,78),(2,4,55),
--             (2,5,32),(3,4,61),(3,5,44),(4,5,93)]
-- [(2,4,55),(1,3,34),(2,5,32),(1,2,12)]

data Graph a = Graph [a] [(a, a, Int)]
              deriving (Show, Eq)

graph84 = Graph [1, 2, 3, 4, 5]
            [(1,2,12),(1,3,34),(1,5,78),(2,4,55),
             (2,5,32),(3,4,61),(3,5,44),(4,5,93)]
												  
paths'							:: (Eq a) => a -> a -> [(a, a, Int)] -> [[a]]
paths' a b xs					| a == b = [[a]]
								| otherwise = concat [map (a :) $ paths' d b $ [x | x <- xs, x /= (c, d, w)]
								              | (c, d, w) <- xs, c == a] ++ 
											  concat [map (a :) $ paths' c b $ [x | x <- xs, x /= (c, d, w)]
											  | (c, d, w) <- xs, d == a]

cycle'							:: (Eq a) => a -> [(a, a, Int)] -> [[a]]
cycle' a xs						= [a : path | e@(e1, e2, e3) <- xs, e1 == a, path <- paths' e2 a [x | x <- xs, x /= e]]

prim							:: (Eq a, Ord a) => Graph a -> [(a, a, Int)]
prim (Graph xs ys)				= let minys = min' ys in prim' [minys] (Graph xs (remove minys ys))
								where
									prim' ts (Graph ns es) | 1 + length ts == length ns = ts -- sol if length of tree = number of nodes - 1
														   | otherwise = -- else prim' with lightest conected edge added to tree
																let mne = (min'' $ filter (not . cycles') $ filter (connected') $ map (flip (:) ts) es)
																in  prim' mne (Graph ns (remove (head mne) es))
									min' (e:es) = foldr (\a@(_, _, a3) b@(_, _, b3) -> if a3 > b3 then b else a) e es -- lightest edge
									min'' (e:es) = foldr (\e1 e2 -> if sum' e1 > sum' e2 then e2 else e1) e es -- lightest path
									sum' e = sum $ map (\(_, _, a3) -> a3) e -- sum of weights of weighted edges
									connected (x':xs') ys'	= not $ any (null) [paths' x' y' ys' | y' <- xs'] -- bool - all nodes connected
									connected' ys' = connected (nodes ys') ys'
									cycles xs' ys' = any ((/=) 0 . length . flip cycle' ys') xs'
									cycles' ys' = cycles (nodes ys') ys'
									nodes e		= foldr (\x xs -> if x `elem` xs then xs else x:xs) 
												[] $ concat $ map (\(a, b, w) -> [a, b]) e
									remove x xs = let (y, y1:ys) = break ((==) x) xs
												  in y ++ ys