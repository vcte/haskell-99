-- --------------------------------------------- --
-- -----------| Haskell Problem #86 -----------| --
-- --------------------------------------------- --

-- (**) Node degree and graph coloration
-- chromatic $ kcolor petersen
-- 3

data Graph a = Graph [a] [(a, a)]
              deriving (Show, Eq)

data Adjacency a = Adj [(a, [a])]
				deriving (Show, Eq)

graphG1 = Graph ['a', 'b', 'c', 'd', 'g', 'h', 'i', 'j']
          [('a', 'g'), ('a', 'h'), ('a', 'i'), ('b', 'g'), ('b', 'h'), ('b', 'j'),
		   ('c', 'g'), ('c', 'i'), ('c', 'j'), ('d', 'h'), ('d', 'i'), ('d', 'j')]

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

-- generates A n Apollonian network graphs
a								:: Int -> Graph Int
a n								= fst $ a' n
								where
									a' 1 = (Graph [1, 2, 3, 4] [(1, 2), (2, 3), (3, 1), (1, 4), (2, 4), (3, 4)],
										    [[1, 2, 4], [2, 3, 4], [1, 3, 4]]) -- list of triangular planes
									a' n = (Graph (v ++ v') (e ++ e'), p')
										where
											(Graph v e, p) = a' (n - 1)
											v' = take (length p) [1 + maximum v ..]
											e' = [(pt, v1) | (p2, v1) <- zip p v', pt <- p2]
											p' = concat $ map (\(pt, vt) -> 
													map (\(a, b) -> [a, b, vt]) $ zip pt $ tail $ cycle pt) $ zip p v'
								
-- converts from graph to adjacency matrix representations
graphToAdj						:: (Eq a) => Graph a -> Adjacency a
graphToAdj (Graph [] _)			= Adj []
graphToAdj (Graph (x:xs) ys)	= Adj ((x, ys >>= f) : zs)
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
canon g 						= Adj $ map (\(a, b) -> (a, sort b 1 maximum)) $ sort x 1 maxv
								where
									Adj x = graphToAdj g
									sort [] _ _ = []
									sort xs n f = let m = f xs in
												   m : sort [x | x <- xs, x /= m] (n + 1) f
									maxv (x:xs) = foldr (\a@(_, a2) b@(_, b2) -> 
															if length a2 > length b2 then a else b) x xs