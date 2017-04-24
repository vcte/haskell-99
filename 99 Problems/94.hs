-- --------------------------------------------- --
-- -----------| Haskell Problem #94 |----------- --
-- --------------------------------------------- --

-- (***) Generate k-regular simple graphs with n nodes
-- length $ regular 6 3
-- 2

data Graph a = Graph [a] [(a, a)]
              deriving (Show, Eq)
			  
data Adjacency a = Adj [(a, [a])]
				deriving (Show, Eq)

regular 						:: Int -> Int -> [Graph Int]
regular n k						| r == 1 || n <= k || n < 0 || k < 0 = []
								| otherwise = 
									map (adjToGraph . fst) $ 
									foldr (\x xs -> if any ((==) (snd x) . snd) xs then xs else x : xs) [] $ 
									zip a $ map canon a
								where
									a = filter (\(Adj a) -> all ((==) k . length . snd) a) $
									    map (graphToAdj . Graph [1..n]) $ perm e q
									e = map (\xs -> (head xs, last xs)) $ perm [1..n] 2
									(q, r) = (n * k) `quotRem` 2
									perm n k = foldr (\x xs -> 
														[i : s | i <- n, s <- xs, i `notElem` s, asc (i : s)])
														[[]] [1..k]
									asc xs = all (uncurry (<)) $ zip xs $ tail xs

graphToAdj						:: (Eq a) => Graph a -> Adjacency a
graphToAdj (Graph [] _)			= Adj []
graphToAdj (Graph (x:xs) ys)	= Adj ((x, ys >>= f) : zs)
								where 
									f (a, b) 
										| a == x = [b]
										| b == x = [a]
										| otherwise = []
									Adj zs = graphToAdj (Graph xs ys)

adjToGraph						:: (Eq a) => Adjacency a -> Graph a
adjToGraph (Adj [])				= Graph [] []
adjToGraph (Adj ((v, a):vs))	= Graph (v : xs) ((a >>= f) ++ ys)
								where
									f x = if (v, x) `elem` ys || (x, v) `elem` ys
									      then []
										  else [(v, x)]
									Graph xs ys = adjToGraph (Adj vs)

canon							:: (Eq a, Ord a) => Adjacency a -> String
canon (Adj a)					= minimum $ map f $ perm n
								where
									n = length a
									v = map fst a
									perm n = foldr (\x xs -> [i : s | i <- [1..n], s <- xs, i `notElem` s]) [[]] [1..n]
									f p = let n = zip v p
										  in show [(snd x, 
													sort id $ map (\x -> 
														snd $ head $ snd $ break ((==) x . fst) n) $ snd $ find a x)
													| x <- sort snd n]
									sort f n = foldr (\x xs -> let (lt, gt) = break ((<) (f x) . f) xs
															   in lt ++ [x] ++ gt) [] n
									find a x = let (xs, ys) = break ((==) (fst x) . fst) a in head ys