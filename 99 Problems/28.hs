-- --------------------------------------------- --
-- -----------| Haskell Problem #29 -----------| --
-- --------------------------------------------- --

-- (*) Sort a list of lists according to length of sublists
-- Prelude > lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o))
-- ((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))

-- quicksort implementation

lsort				:: [[a]] -> [[a]]
lsort []			= []
lsort [x]			= [x]
lsort xs			= lsort [l | l <- tail xs, length l < length p] ++
					  [p] ++
					  lsort [g | g <- tail xs, length g >= length p]
					  where
						p = head xs
						
lsort'				:: [[[a]]] -> [[[a]]]
lsort' []			= []
lsort' [x]			= [x]
lsort' xs			= lsort' [l | l <- tail xs, length l < length p] ++
					  [p] ++
					  lsort' [g | g <- tail xs, length g >= length p]
					  where
						p = head xs
						
lfsort				:: [[a]] -> [[a]]
lfsort xs			= concat $ lsort' xs2
						where
							xs2 = [[x | x <- xs, length x == i]
									| i <- [1..(maximum $ map length xs)]]