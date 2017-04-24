-- --------------------------------------------- --
-- -----------| Haskell Problem #49 -----------| --
-- --------------------------------------------- --

-- (**) Gray codes
-- Prelude > gray 3
-- ((0, 0, 0), (0, 0, 1), (0, 1, 1), (0, 1, 0), 
--  (1, 1, 0), (1, 1, 1), (1, 0, 1), (1, 0, 0))

gray 				:: Int -> [[Int]]
gray 0				= [[]]
gray n				= (map (0:) prev) ++
					  (map (1:) $ reverse prev)
					  where
						prev = gray (n - 1)