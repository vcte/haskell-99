-- --------------------------------------------- --
-- -----------| Haskell Problem #98 -----------| --
-- --------------------------------------------- --

-- (***) Nonograms
-- display puzzle $ head $ nonogram puzzle
-- |_|X|X|X|_|_|_|_| 3 ...

puzzle :: ([[Int]], [[Int]]) -- 8 x 9, ~ 0.12 s
puzzle = ([[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]],
		  [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]])
		  
puzzle2 :: ([[Int]], [[Int]]) -- 20 x 10, ~159 s, ~6.25e9 bytes | ~23 s, ~6.25e9 bytes
puzzle2 = ([[6],[3,1,3],[1,3,1,3],[3,14],[1,1,1], 
            [1,1,2,2],[5,2,2],[5,1,1],[5,3,3,3],[8,3,3,3]],
           [[4],[4],[1,5],[3,4],[1,5],[1],[4,1],[2,2,2],
            [3,3],[1,1,2],[2,1,1],[1,1,2],[4,1],[1,1,2],
            [1,1,1],[2,1,2],[1,1,1],[3,4],[2,2,1],[4,1]])
		  
puzzle3 :: ([[Int]], [[Int]]) -- 20 x 20, >24 hrs
puzzle3 = ([[3],[5],[3,1],[2,1],[3,3,4],[2,2,7],[6,1,1],[4,2,2],[1,1],[3,1],
            [6],[2,7],[6,3,1],[1,2,2,1,1],[4,1,1,3],[4,2,2],[3,3,1],[3,3],[3],[2,1]],
           [[2],[1,2],[2,3],[2,3],[3,1,1],[2,1,1],[1,1,1,2,2],[1,1,3,1,3],[2,6,4],[3,3,9,1],
		    [5,3,2],[3,1,2,2],[2,1,7],[3,3,2],[2,4],[2,1,2],[2,2,1],[2,2],[1],[1]])
		  
nonogram				:: ([[Int]], [[Int]]) -> [[String]]
nonogram ([], _)		= [[]]
nonogram ((x:xs), ys)	= [row : sol | sol <- nonogram (xs, ys), row <- perm x (length ys), fit (row : sol) ys]
						where
							perm [] _ = [[]]
							perm (x:xs) n = [replicate s '_' ++ replicate x 'X' ++ replicate (n - s - x - length p) '_' ++ p
											| s <- [0 .. n - x], 
											  p <- map (\x -> if null x then x else '_':x) $ perm xs (n - s - x - 1)]
							fit xs ys = all (uncurry fit') $ zip [reverse $ map (flip (!!) c) xs | c <- [0..(length ys) - 1]]
														   $ map reverse ys
							fit' c ls = c `elem` (map (take (length c)) $ perm ls (1 + length ys))
							brk x = let (xs, xs') = break ((==) 'X') x;
										(ys, zs)  = break ((==) '_') xs'
									in (xs, ys, zs)

display					:: ([[Int]], [[Int]]) -> [String] -> IO ()
display	(xs, ys) zs		= putStrLn $ str (xs, ys) zs
						where
							str ([], []) _			= ""
							str ([], ys) zs			| all null ys = ""
													| otherwise =
													  (concat $ map (\x -> if null x then "  " else ' ' : (show $ head x)) ys) ++ 
													  "\n" ++ str ([], (map (\x -> if null x then x else tail x) ys)) zs
							str	(x:xs, ys) (z:zs)	= (concat $ map (\x -> '|' : [x]) z) ++ "|" ++ 
													  (concat $ map (\x -> ' ' : show x) x) ++ "\n" ++ str (xs, ys) zs