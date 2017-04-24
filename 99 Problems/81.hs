-- --------------------------------------------- --
-- -----------| Haskell Problem #81 -----------| --
-- --------------------------------------------- --

-- (**) Path from one node to another (directional)
-- paths 1 4 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
-- [[1,2,3,4], [1,3,4]]

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

paths							:: (Eq a) => a -> a -> [(a, a)] -> [[a]]
paths a b xs					| a == b = [[a]]
								| otherwise = concat [map (a :) $ paths d b $ remove (c, d) xs
								              | (c, d) <- xs, c == a]
								where
									remove x xs = let (y, y1:ys) = break ((==) x) xs
												  in y ++ ys