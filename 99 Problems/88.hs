-- --------------------------------------------- --
-- -----------| Haskell Problem #88 -----------| --
-- --------------------------------------------- --

-- (**) Connected components
-- components graph1
-- [Graph ['b', 'c', 'f', 'k'] [('b', 'c'), ('b', 'f'), ('c', 'f'), ('f', 'k')],
--  Graph ['d'] [],
--  Graph ['g', 'h'] [('g', 'h')]]

data Graph a = Graph [a] [(a, a)]
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

-- bidirectional path finding
paths'							:: (Eq a) => a -> a -> [(a, a)] -> [[a]]
paths' a b xs					| a == b = [[a]]
								| otherwise = concat [map (a :) $ paths' d b $ [x | x <- xs, x /= (c, d)]
								              | (c, d) <- xs, c == a] ++ 
											  concat [map (a :) $ paths' c b $ [x | x <- xs, x /= (c, d)]
											  | (c, d) <- xs, d == a]

-- splits graph into list of connected components
components						:: (Eq a) => Graph a -> [Graph a]
components (Graph [] _)			= []
components (Graph (x:xs) ys)	= Graph vs es : components (Graph vs' es')
								where
									vs = x : [v | v <- xs, not $ null $ paths' x v ys]
									es = [e | e <- ys, fst e `elem` vs || snd e `elem` vs]
									vs' = [v | v <- xs, not $ elem v vs]
									es' = [e | e <- ys, not $ elem e es]