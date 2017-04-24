-- --------------------------------------------- --
-- -----------| Haskell Problem #89 -----------| --
-- --------------------------------------------- --

-- (**) Bipartite graphs
-- bipartite kb 3 5
-- True
-- bipartite petersen
-- False

data Graph a = Graph [a] [(a, a)]
              deriving (Show, Eq)

data Adjacency a = Adj [(a, [a])]
				deriving (Show, Eq)

petersen = Graph ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
		   [('a', 'b'), ('a', 'e'), ('a', 'f'), ('b', 'c'), ('b', 'g'), 
		    ('c', 'd'), ('c', 'h'), ('d', 'e'), ('d', 'i'), ('e', 'j'), 
			('f', 'h'), ('f', 'i'), ('g', 'i'), ('g', 'j'), ('h', 'j')]

herschel = Graph [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
		   [(1, 2), (1, 3), (1, 4), (1, 5), (2, 6), (2, 8), (3, 7), (3, 8), (6, 11),
		    (4, 6), (4, 10), (5, 7), (5, 10), (7, 9), (8, 9), (8, 11), (9, 10), (10, 11)]
			
-- generates P_n path graph, with n nodes
p								:: Int -> Graph Int
p n								| n > 0     = Graph [1 .. n] $ zip [1 .. (n - 1)] [2 .. n]
								| otherwise = Graph [] []
			
-- generates L_n line graphs, with 2 * n nodes
l								:: Int -> Graph Int
l n								= let Graph xs ys = p n; t = (+) n
								  in Graph [1 .. 2 * n] (ys ++ (zip xs $ map t xs) ++ (map (\(a, b) -> (t a, t b)) ys))

-- generates C_n cycle graph, with n nodes
c								:: Int -> Graph Int
c n								| n >= 3    = Graph [1..n] $ ((n, 1) :) $ zip [1..(n - 1)] [2..n]
								| otherwise = Graph [] []

-- generates W_n wheel graph, with n + 1 nodes
w								:: Int -> Graph Int
w n								= let Graph xs ys = c n
								  in Graph (xs ++ [(n + 1)]) (ys ++ (zip [1..n] $ repeat (n + 1)))
								
-- generates K_n complete graph, with n nodes
k								:: Int -> Graph Int
k n								| n > 0     = let Graph xs ys = k (n - 1)
											  in Graph (xs ++ [n]) (ys ++ (zip [1..(n - 1)] $ repeat n))
								| otherwise = Graph [] []

-- generates K m n complete bipartite graphs, parititioned into m and n nodes
kb								:: Int -> Int -> Graph Int
kb m n 							| n > 0     = Graph [1..(m + n)] $ zip ([1..m] >>= replicate n) 
																	   (concat $ replicate m [m + 1 .. m + n])
								| otherwise = Graph [] []

-- generates S n star graphs
s								:: Int -> Graph Int
s k								= kb 1 k
								
-- generates Q n hypercube graphs
q								:: Int -> Graph Int
q n								| n >= 1    = let Graph xs ys = q (n - 1);
												  t = (+) (2^(n-1))
											  in Graph ([1 .. 2 ^ n]) (ys ++ 
																	   (map (\(a, b) -> (t a, t b)) ys) ++
																	   (zip xs $ map t xs))
								| n == 0    = Graph [1] []

-- bidirectional path finding
paths'							:: (Eq a) => a -> a -> [(a, a)] -> [[a]]
paths' a b xs					| a == b = [[a]]
								| otherwise = concat [map (a :) $ paths' d b $ [x | x <- xs, x /= (c, d)]
								              | (c, d) <- xs, c == a] ++ 
											  concat [map (a :) $ paths' c b $ [x | x <- xs, x /= (c, d)]
											  | (c, d) <- xs, d == a]

-- bidirectional cycles
cycle'							:: (Eq a) => a -> [(a, a)] -> [[a]]
cycle' a xs						= [a : path | e@(e1, e2) <- xs, e1 == a, path <- paths' e2 a [x | x <- xs, x /= e]]

-- converts from graph to adjacency matrix representations
graphToAdj						:: (Eq a) => Graph a -> Adjacency a
graphToAdj (Graph [] _)			= Adj []
graphToAdj (Graph (x:xs) ys)	= Adj ((x, concat $ map f ys) : zs)
								where 
									f (a, b) 
										| a == x = [b]
										| b == x = [a]
										| otherwise = []
									Adj zs = graphToAdj (Graph xs ys)

-- produces graph coloration using Welch-Powell algorithm
kcolor							:: (Eq a, Ord a) => Graph a -> [(a, Int)]
kcolor g						= kcolor' x [] 1
								where
									Adj x = canon g
									kcolor' [] ys _ = ys
									kcolor' xs ys n = let ys' = color xs ys n
													  in kcolor' [x | x <- xs, notElem (fst x, n) ys']
																 ys'
																 (n + 1)
									color []          ys n = ys
									color ((v, e):xs) ys n = if any (\x -> (x, n) `elem` ys) e
															 then color xs ys n
															 else color xs ((v, n) : ys) n

-- determines chromatic number, given graph coloration
chromatic						:: [(a, Int)] -> Int
chromatic x						= length $ foldr (\(a, n) xs -> if n `elem` xs then xs else n : xs) [] x

-- produces graph sorted by node degree
canon							:: (Eq a, Ord a) => Graph a -> Adjacency a
canon g 						= Adj h
								where
									Adj x = graphToAdj g
									h = map (\(a, b) -> (a, sort' b 1)) $ sort x 1
									sort [] _ = []
									sort xs n = let m = maxv xs in
												   m : sort [x | x <- xs, x /= m] (n + 1)
									sort' [] _ = []
									sort' xs n = let m = maximum xs in
													m : sort' [x | x <- xs, x /= m] (n + 1)
									maxv (x:xs) = foldr (\a@(a1, _) b@(b1, _) -> if a1 > b1 then a else b) x xs

-- determines whether graph is bipartite
bipartite						:: (Eq a) => Graph a -> Bool
bipartite g						= decomp x [] []
								where 
									Adj x = graphToAdj g
									decomp [] _ _ = True
									decomp ((v, e):xs) s t
										| not $ any (flip elem s) e = decomp xs (v:s) t
										| not $ any (flip elem t) e = decomp xs s (v:t)
										| otherwise = False

-- graph is bipartite iff no odd-length cycles
bipartite' 						:: (Eq a) => Graph a -> Bool
bipartite' (Graph xs ys)		= all (\(x:xs) -> length xs `mod` 2 == 0) $ concat $ map (flip cycle' ys) xs

-- graph is bipartite iff 2-colorable
bipartite''						:: (Eq a, Ord a) => Graph a -> Bool
bipartite'' g					= (chromatic $ kcolor g) <= 2