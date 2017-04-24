-- --------------------------------------------- --
-- -----------| Haskell Problem #97 -----------| --
-- --------------------------------------------- --

-- (**) Sudoku
-- display $ sudoku puzzle
-- -------------------
-- |5|3|4|6|7|8|9|1|2| ...

data Graph a = Graph [a] [(a, a)]
              deriving (Show, Eq)

data Adjacency a = Adj [(a, [a])]
				deriving (Show, Eq)

puzzle = [[5, 3, 0, 0, 7, 0, 0, 0, 0], -- Wikipedia puzzle
          [6, 0, 0, 1, 9, 5, 0, 0, 0],
		  [0, 9, 8, 0, 0, 0, 0, 6, 0],
		  [8, 0, 0, 0, 6, 0, 0, 0, 3],
		  [4, 0, 0, 8, 0, 3, 0, 0, 1],
		  [7, 0, 0, 0, 2, 0, 0, 0, 6],
		  [0, 6, 0, 0, 0, 0, 2, 8, 0],
		  [0, 0, 0, 4, 1, 9, 0, 0, 5],
		  [0, 0, 0, 0, 8, 0, 0, 7, 9]]

puzzle2 = [[0, 0, 4, 8, 0, 0, 0, 1, 7], -- haskell.org puzzle
		   [6, 7, 0, 9, 0, 0, 0, 0, 0],
		   [5, 0, 8, 0, 3, 0, 0, 0, 4],
		   [3, 0, 0, 7, 4, 0, 1, 0, 0],
		   [0, 6, 9, 0, 0, 0, 7, 8, 0],
		   [0, 0, 1, 0, 6, 9, 0, 0, 5],
		   [1, 0, 0, 0, 8, 0, 3, 0, 6],
		   [0, 0, 0, 0, 0, 6, 0, 9, 1],
		   [2, 4, 0, 0, 0, 1, 5, 0, 0]]
		  
puzzle3 = [[0, 0, 8, 0, 0, 0, 0, 2, 0], -- WebSudoku puzzle
           [5, 0, 0, 0, 0, 7, 0, 3, 0],
		   [0, 1, 0, 0, 0, 8, 4, 0, 0],
		   [0, 0, 0, 1, 6, 0, 5, 0, 0],
		   [3, 0, 0, 0, 8, 0, 0, 0, 1],
		   [0, 0, 9, 0, 3, 4, 0, 0, 0],
		   [0, 0, 2, 4, 0, 0, 0, 9, 0],
		   [0, 4, 0, 8, 0, 0, 0, 0, 2],
		   [0, 3, 0, 0, 0, 0, 6, 0, 0]]
		   
puzzle4 = [[0, 0, 0, 0, 0, 0, 0, 0, 0], -- near worst-case
		   [0, 0, 0, 0, 0, 3, 0, 8, 5], -- sudoku'' takes ~3600 s, ~89e9 bytes
		   [0, 0, 1, 0, 2, 0, 0, 0, 0],
		   [0, 0, 0, 5, 0, 7, 0, 0, 0],
		   [0, 0, 4, 0, 0, 0, 1, 0, 0],
		   [0, 9, 0, 0, 0, 0, 0, 0, 0],
		   [5, 0, 0, 0, 0, 0, 0, 7, 3],
		   [0, 0, 2, 0, 1, 0, 0, 0, 0],
		   [0, 0, 0, 0, 4, 0, 0, 0, 9]]
		  
puzzle5 = [[0, 0, 5, 3, 0, 0, 0, 0, 0], -- Inkala 2010 puzzle
		   [8, 0, 0, 0, 0, 0, 0, 2, 0],
		   [0, 7, 0, 0, 1, 0, 5, 0, 0],
		   [4, 0, 0, 0, 0, 5, 3, 0, 0],
		   [0, 1, 0, 0, 7, 0, 0, 0, 6],
		   [0, 0, 3, 2, 0, 0, 0, 8, 0],
		   [0, 6, 0, 5, 0, 0, 0, 0, 9],
		   [0, 0, 4, 0, 0, 0, 0, 3, 0],
		   [0, 0, 0, 0, 0, 9, 7, 0, 0]]
		  
-- solves sudoku puzzle using rules of inference
sudoku					:: [[Integer]] -> [[Integer]]
sudoku x				= unpack $ solve $ pack x
						where
							pack x = map (map (\n -> if n == 0
													 then [1..9]
													 else [n])) x
							unpack x = map (map (\n -> if length n /= 1
													   then 0
													   else head n)) x
							solve p = let p2 = horiz p;
										  p3 = vert p2;
										  p4 = block p3;
										  p5 = infer p4
									  in
										if p == p5
										then p
										else solve p5
							horiz p = map (\r -> foldr (\i r -> if length (r !! i) == 1 then
									  map (\x -> if length x == 1 then x else remove (head (r !! i)) x) r 
										else r) r [0..8]) p
							vert p = foldr (\j p -> 
										foldr (\i p -> 
											if length (p !! j !! i) == 1 
											then
												foldr (\y p -> 
													map (\r -> 
														if length (r !! i) == 1 
														then r 
														else take i r ++ 
															remove (head (p !! j !! i)) (r !! i) :
															drop (i + 1) r)
													p) p [0..8] 
											else p) 
										p [0..8])
									 p [0..8]
							block p = let blk = [0..2] >>= \x -> [0..2] >>= \y -> [(x, y)] in
										foldr (\(c, r) p -> 
											foldr (\(x, y) p ->
												if length (p !! (r + y) !! (c + x)) == 1
												then 
													let e = head (p !! (r + y) !! (c + x))
													in
														foldr (\(x, y) p ->
															if length (p !! (r + y) !! (c + x)) == 1
															then p
															else 
																let l = p !! (r + y)
																in 
																	take (r + y) p ++
																	[take (c + x) l ++
																	 remove e (l !! (c + x)) :
																	 drop (c + x + 1) l] ++
																	drop (r + y + 1) p)
														p blk
												else p)
											p blk)
										p ([0, 3, 6] >>= \x -> [0, 3, 6] >>= \y -> [(x, y)])
							infer p = let blk = [0..2] >>= \x -> [0..2] >>= \y -> [(x, y)];
										  blks = ([0, 3, 6] >>= \x -> [0, 3, 6] >>= \y -> [(x, y)]) in
										foldr (\(c, r) p ->
											foldr (\(x, y) p ->
												if length (p !! (r + y) !! (c + x)) == 1
												then p
												else 
													foldr (\i p ->
														if all (\(x', y') -> (x == x' && y == y') ||
															i `notElem` (p !! (r + y') !! (c + x'))) blk
														then 
															let l = p !! (r + y)
																in 
																	take (r + y) p ++
																	[take (c + x) l ++
																	 [i] :
																	 drop (c + x + 1) l] ++
																	drop (r + y + 1) p
														else p)
													p (p !! (r + y) !! (c + x)))
											p blk)
										p blks
							remove y [] = []
							remove y (x:xs) = if x == y then xs else x : remove y xs

-- solves sudoku using graph coloration (doesn't do 9 coloring)
sudoku'						:: [[Integer]] -> [[Integer]]
sudoku' x					= sol
							where
								g = Graph vs es
								vs = [1..9] >>= \x -> [1..9] >>= \y -> [10*x + y]
								es = foldr (\x xs -> if x `elem` xs then xs else x : xs) [] $ 
									 ([1..9] >>= \x -> [1..8] >>= \y -> [y+1..9] >>= \z -> [(10*x+y, 10*x+z)]) ++
								     ([1..8] >>= \x -> [x+1..9] >>= \y -> [1..9] >>= \z -> [(10*x+z, 10*y+z)]) ++
									 (blks >>= \x -> [0..7] >>= \y -> [y+1..8] >>= \z -> [(x+(b!!y), x+(b!!z))])
								blks = [11, 14, 17, 41, 44, 47, 71, 74, 77]
								b = [0, 1, 2, 10, 11, 12, 20, 21, 22]
								ys = [1..9] >>= \r -> [1..9] >>= \c -> let n = x !! (r - 1) !! (c - 1) in
										if n == 0 then [] else [(r*10+c, n)]
								a = kcolor g ys
								sol = foldr (\(x, c) xs -> update xs (x `div` 10) (x `mod` 10) c) 
											(replicate 9 (replicate 9 0)) a
								update xs r c e = let l = xs !! (r - 1)
												  in take (r - 1) xs ++ [take (c - 1) l ++ e : drop c l] ++ drop r xs
							
-- solves sudoku using backtracking
sudoku''					:: [[Integer]] -> [[[Integer]]]
sudoku'' x					= solve x' 0 0
							where
								x' = sudoku x
								solve x 9 0 = [x]
								solve x r c = let (r', c') = next (r, c)
											  in if x !! r !! c /= 0
											     then solve x r' c'
												 else
													concat [solve (set x r c n) r' c' 
														   | n <- [1..9], fit x r c n]
								next (r, c) = let (d, m) = (c + 1) `divMod` 9
											  in (r + d, m)
								set x r c n = let l = x !! r in 
												take r x ++
												[take c l ++ n : drop (c + 1) l] ++
												drop (r + 1) x
								fit x r c n = (all ((/=) n) (x !! r)) &&
											  (all (\j -> x !! j !! c /= n) [0..8]) &&
											  (all (\(r', c') -> x !! (3 * (r `div` 3) + r') !! (3 * (c `div` 3) + c') /= n) b)
								b = [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2)]
							
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
kcolor							:: (Eq a, Ord a) => Graph a -> [(a, Integer)] -> [(a, Integer)]
kcolor g ys						= kcolor' xs ys 1
								where
									Adj x = canon g
									xs = filter (\(v, e) -> not $ any (\(n, c) -> v == n) ys) x
									kcolor' [] ys _ = ys
									kcolor' xs ys n = let ys' = color xs ys n
													  in kcolor' [x | x <- xs, notElem (fst x, n) ys']
																 ys'
																 (n + 1)
									color []          ys n = ys
									color ((v, e):xs) ys n = if any (\x -> (x, n) `elem` ys) (v : e)
															 then color xs ys n
															 else color xs ((v, n) : ys) n

-- produces graph sorted by node degree
canon							:: (Eq a, Ord a) => Graph a -> Adjacency a
canon g 						= Adj h
								where
									Adj x = graphToAdj g
									h = map (\(a, b) -> (a, sort' b 1)) $ sort x 1
									sort [] _ = []
									sort xs n = let m = minv xs in
												   m : sort [x | x <- xs, x /= m] (n + 1)
									sort' [] _ = []
									sort' xs n = let m = minimum xs in
													m : sort' [x | x <- xs, x /= m] (n + 1)
									minv (x:xs) = foldr (\a@(a1, _) b@(b1, _) -> if a1 > b1 then b else a) x xs
							
display 				:: [[Integer]] -> IO ()
display x				= putStrLn $ str x
						where
							str [] = replicate 19 '-'
							str (x:xs) = (replicate 20 '-') ++ "\n" ++ 
										 (concat $ map (\n -> "|" ++ show n) x) ++ "|\n" ++
										 (str xs)
										 
display'				:: [[Integer]] -> IO ()
display' x				= putStrLn $ edge 2 ++ str x 0
						where
							str [] _ = ""
							str (x:xs) n = disp' x ++ edge n ++ str xs (n + 1)
							disp' [] = "\n"
							disp' (x:y:z:xs) = show' x ++ "  " ++ show' y ++ "  " ++ show' z ++ end xs ++ disp' xs
							end x = if x == [] then "" else " | "
							edge n = if n `mod` 3 /= 2 
									 then replicate 8 ' ' ++ "|" ++ replicate 9 ' ' ++ "|" ++ replicate 8 ' ' ++ "\n"
									 else replicate 8 '-' ++ "+" ++ replicate 9 '-' ++ "+" ++ replicate 8 '-' ++ "\n"
							show' n = if n == 0 then "." else show n