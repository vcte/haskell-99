-- --------------------------------------------- --
-- -----------| Haskell Problem #90 -----------| --
-- --------------------------------------------- --

-- (**) Eight queens problem
-- length $ queens 8
-- 92

queens							:: Int -> [[Int]]
queens n						= filter (f 0) sols
								where
									sols = foldr (\_ xs -> filter (not . null) $ concat $ map
										(\x -> map (\ys -> if x `elem` ys then [] else (x : ys)) xs)
										[1..n]) [[]] [1..n]
									f i xs  | length xs == i + 1 = True
											| otherwise = if (e xs [p - i .. p - i + n] p) ||
															 (e xs (reverse [p + i - n .. p + i]) p)
														  then False
														  else f (i + 1) xs
											where
												p = xs !! i
									e [] _ _ = False
									e (x:xs) (y:ys) p = (p /= x && x == y) || e xs ys p