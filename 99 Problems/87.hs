-- --------------------------------------------- --
-- -----------| Haskell Problem #87 -----------| --
-- --------------------------------------------- --

-- (**) Depth-first graph traversal
-- traversal graph1 'b'
-- ['b', 'c', 'f', 'k']

data Graph a = Graph [a] [(a, a)]
              deriving (Show, Eq)

data Adjacency a = Adj [(a, [a])]
				deriving (Show, Eq)

graph1 = Graph ['b','c','d','f','g','h','k']
         [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]

petersen = Graph ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
		   [('a', 'b'), ('a', 'e'), ('a', 'f'), ('b', 'c'), ('b', 'g'), 
		    ('c', 'd'), ('c', 'h'), ('d', 'e'), ('d', 'i'), ('e', 'j'), 
			('f', 'h'), ('f', 'i'), ('g', 'i'), ('g', 'j'), ('h', 'j')]

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

-- traverses graph from starting node in depth first manner
traversal						:: (Eq a) => Graph a -> a -> [a]
traversal g	a					= traversal' x a []
								where
									Adj x = graphToAdj g
									traversal' [] _ ys  = ys
									traversal' xs a ys = let xs' = filter ((==) a . fst) xs in
												if null xs' || (elem (fst $ head xs') ys)
												then ys
												else foldr (\x -> traversal' [x | x <- xs, fst x /= a] x)
														   (ys ++ [fst $ head xs']) $ snd $ head xs'