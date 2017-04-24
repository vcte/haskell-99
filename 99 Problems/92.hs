-- --------------------------------------------- --
-- -----------| Haskell Problem #92 -----------| --
-- --------------------------------------------- --

-- (***) Von Koch's conjecture
-- head $ koch [(1,6),(2,6),(3,6),(4,6),(5,6),(5,7),(5,8),(8,9),(5,10),(10,11),(11,12),(11,13),(13,14)]
-- [6,7,8,9,3,4,10,11,5,12,2,13,14,1]

graph1 = [(1,6),(2,6),(3,6),(4,6),(5,6),(5,7),(5,8),(8,9),(5,10),(10,11),(11,12),(11,13),(13,14)]

koch						:: [(Int, Int)] -> [[Int]]
koch xs						= filter (not . null) $ koch' xs (nodes xs) (replicate (length xs) 0) []
							where
								nodes xs = foldr (\x xs -> let (lt, gt) = span ((>) x) xs in lt ++ (x : gt)) [] $
										   foldr (\x xs -> if x `elem` xs then xs else x : xs) [] $
										   xs >>= (\(a, b) -> [a, b])
								update es nl = map (\(a, b) -> if a > length nl || b > length nl 
															   then 0 else abs (nl !! (a - 1) - nl !! (b - 1))) es
								collide [] = False
								collide (l:el) = (l /= 0 && l `elem` el) || collide el
								koch' es ns el nl | collide el = [[]]
												  | length ns == length nl = [nl]
												  | otherwise = concat [koch' es ns (update es (n : nl)) (n : nl)
																	   | n <- [1 .. length ns], n `notElem` nl]
													
display						:: [Int] -> IO ()
display xs					= putStrLn $ replace s xs
							where
								replace []    xs = []
								replace (c:s) xs = if c == '|' && s !! 1 == '|'
												   then let a = toInt $ head s
												        in "|" ++ show (xs !! a - 1) ++ "|" ++ replace (drop 2 s) xs
												   else 
														if toInt c /= -1
														then let a = toInt c
															 in show (el !! a - 1) ++ replace s xs
														else c : replace s xs
								index s c = let (xs, ys) = break ((==) c) s
											in if xs == s then -1 else length xs
								conv = "0123456789"
								toInt c = index conv c
								s = ",-.   ,-. 1 ,-.   \n" ++ 
									"|1|   |5|---|4|   \n" ++ 
									"`-'   `-'   `-'   \n" ++ 
									"6|     |2         \n" ++ 
									",-.   ,-.   ,-.   \n" ++ 
									"|7|---|3|---|6|   \n" ++ 
									"`-' 4 `-' 3 `-'   \n" ++ 
									"5|                \n" ++ 
									",-.               \n" ++ 
									"|2|               \n" ++ 
									"`-'               \n"
								el = update es xs
								es = [(5, 4), (5, 3), (3, 6), (3, 7), (2, 7), (1, 7)]
								update es nl = map (\(a, b) -> if a > length nl || b > length nl 
															   then 0 else abs (nl !! (a - 1) - nl !! (b - 1))) es