-- --------------------------------------------- --
-- -----------| Haskell Problem #50 -----------| --
-- --------------------------------------------- --

-- (***) Huffman codes
-- Prelude > huffman ((A,45),(B,13),(C,12),(D,16),(E,9),(F,5))
-- ((A,"0"),(B,"101"),(C,"100"),(D,"111"),(E,"1101"),(F,"1100"))

huffman 				:: [(String, Int)] -> [(String, String)]
huffman	xs				= huffcode $ hufftree tree leaf
						where
							tree = [(s, n, "", "") | (s, n) <- xs]
							leaf = map fst xs
							
-- recursively generates binary tree from characters to encode and weights
hufftree				:: [(String, Int, String, String)] -> [String] ->
						   [(String, Int, String, String)]
hufftree xs	ns			| length ns == 1 = xs
						| otherwise = hufftree (mn : xs) (mn1 : ns3)
							where
								(ns2, m) = getMin xs ns
								(ns3, n) = getMin xs ns2
								mn1 = fst m ++ fst n
								mn = (mn1, snd m + snd n, fst m, fst n)
								minm = \xs x y -> let (x1, x2, x3, x4) = find xs x; 
													  (y1, y2, y3, y4) = find xs y
												in if x2 < y2 then x else y
								getMin = \xs ns -> let m = foldl (minm xs) (head ns) ns;
													  (m1, m2, m3, m4) = find xs m
												in (filter ((/=) m) ns, (m1, m2))
								find = \xs x -> head $ filter (\(a, b, c, d) -> a == x) xs
						   
-- generates huffman code from binary tree
huffcode				:: [(String, Int, String, String)] ->
						   [(String, String)]
huffcode xs				= huffcode' xs (head xs)

huffcode'				:: [(String, Int, String, String)] ->
						    (String, Int, String, String)  -> [(String, String)]
huffcode' xs x			| length a == 1 = [(a, "")]
						| otherwise = left ++ right
						where
							(a, b, c, d) = x
							left 	= [(a, '0' : b) | (a, b) <- huffcode' xs $ find xs c]
							right	= [(a, '1' : b) | (a, b) <- huffcode' xs $ find xs d]
							find 	= \xs x -> head $ filter (\(a, b, c, d) -> a == x) xs