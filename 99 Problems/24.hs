-- --------------------------------------------- --
-- -----------| Haskell Problem #24 -----------| --
-- --------------------------------------------- --

-- (*) Lotto: draw N different random numbers from the set 1..M
-- Prelude > lotto 6 49
-- (23 1 17 33 21 37)

import System.Random

lotto					:: Int -> Int -> IO [Int]
lotto 0 m				= return []
lotto n m				= mapM (\x -> fmap ((!!) [1..m]) $ getStdRandom $ randomR (0, m - 1)) [1..n]