-- --------------------------------------------- --
-- -----------| Haskell Problem #80 -----------| --
-- --------------------------------------------- --

-- (***) Graph conversions

data Graph a = Graph [a] [(a, a)]
              deriving (Show, Eq)
			  
data Adjacency a = Adj [(a, [a])]
				deriving (Show, Eq)

data Friendly a = Edge [(a, a)]
				deriving (Show, Eq)

graph1 = Graph ['b','c','d','f','g','h','k']
         [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]
		 
adj1   = Adj [('b', ['c', 'f']), ('c', ['b', 'f']), ('d', []), 
          ('f', ['b', 'c', 'k']), ('g', ['h']), ('h', ['g']), ('k', ['f'])]
		  
friendly1 = Edge [('b','c'),('b','f'),('c','f'),('f','k'),('g','h'), ('d', 'd')]

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
									
graphToFri						:: (Eq a) => Graph a -> Friendly a
graphToFri (Graph [] _)			= Edge []
graphToFri (Graph xs ys)		= Edge (ys ++ zip g g)
								where
									g = filter (\x -> all (\(a, b) -> x /= a && x /= b) ys) xs
									
friToGraph						:: (Eq a) => Friendly a -> Graph a
friToGraph (Edge [])			= Graph [] []
friToGraph (Edge vs)			= Graph xs ys
								where
									xs = foldr acc [] $ concat $ map (\(a, b) -> [a, b]) vs
									ys = filter (uncurry (/=)) vs
									acc x xs = if x `elem` xs then xs else x : xs
									
adjToFri						:: (Eq a) => Adjacency a -> Friendly a
adjToFri						= graphToFri . adjToGraph

friToAdj						:: (Eq a) => Friendly a -> Adjacency a
friToAdj						= graphToAdj . friToGraph