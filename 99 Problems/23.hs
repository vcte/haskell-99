-- --------------------------------------------- --
-- -----------| Haskell Problem #23 -----------| --
-- --------------------------------------------- --

-- (*) Extract a given number of random elements from a list
-- Prelude > randsel '(a b c d e f g h) 3
-- (E D A)

import System.Random

randsel					:: [a] -> Int -> IO [a]
randsel xs 0			= return []
randsel xs n			= mapM (\x -> fmap ((!!) xs) $ getStdRandom $ randomR (0, (length xs) - 1)) [1..n]