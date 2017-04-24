-- --------------------------------------------- --
-- -----------| Haskell Problem #91 -----------| --
-- --------------------------------------------- --

-- (**) Knight's tour problem
-- head $ knights 8 (1,1)
-- [(2,7),(3,5),(5,6),(4,8),(3,6),(4,4),(6,5),(4,6)..]

knights							:: Int -> (Int, Int) -> [[(Int, Int)]]
knights n (x, y)				= knights' n (x, y) []
								where
									knights' n (x, y) xs | length xs == n*n - 1 = [[(x, y)]]
														 | otherwise = 
										map ((x, y) : ) $ filter (not . null) $ concat [knights' n (a, b) ((x, y) : xs)
											| (a, b) <- jumps x y, a > 0, b > 0, a <= n, b <= n, (a, b) `notElem` xs]
									jumps x y = [(x - 2, y - 1), (x - 1, y - 2), (x - 2, y + 1), (x - 1, y + 2),
									             (x + 2, y - 1), (x + 1, y - 2), (x + 2, y + 1), (x + 1, y + 2)]
												 
cknights						:: Int -> [[(Int, Int)]]
cknights n						= cknights' n (1, 1) []
								where
									cknights' n (x, y) xs | (x, y) == (1, 1) && (not $ null xs) = [[(1, 1)]]
														  | otherwise = 
										map ((x, y) : ) $ filter (not . null) $ concat [cknights' n (a, b) ((x, y) : xs)
											| (a, b) <- jumps x y, a > 0, b > 0, a <= n, b <= n, (a, b) `notElem` xs]
									jumps x y = [(x - 2, y - 1), (x - 1, y - 2), (x - 2, y + 1), (x - 1, y + 2),
									             (x + 2, y - 1), (x + 1, y - 2), (x + 2, y + 1), (x + 1, y + 2)]