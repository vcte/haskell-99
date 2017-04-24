-- --------------------------------------------- --
-- -----------| Haskell Problem #93 -----------| --
-- --------------------------------------------- --

-- (***) Arithmetic puzzle
-- mapM_ putStrLn $ puzzle [2,3,5,7,11]
-- 2 = 3-(5+7-11) ...

data Tree = Branch (String) (Tree) (Tree) | Leaf Int
		deriving (Show, Eq)

puzzle						:: [Int] -> [String]
puzzle xs					= filter (not . null) $ puzzle' symb leaf
							where
								symb = take (length xs) ['a' ..]
								leaf = zip symb $ map (\x -> Leaf x) xs
								ops = ["+", "-", "*", "/"]
								puzzle' xs ys | length xs == 2 = if (eval $ snd (ys !! 0)) == (eval $ snd (ys !! 1))
																 then [str $ snd $ head $ grow ys (xs !! 0) (xs !! 1) "="]
																 else []
											  | otherwise = 
												concat [puzzle' (remove xs (i + 1)) (grow ys (xs !! (i - 1)) (xs !! i) op)
													   | i <- [1 .. (length xs) - 1], op <- ops]
								remove xs i = take (i - 1) xs ++ drop i xs
								grow tree a b op = let (xs, y:ys) = break ((==) a . fst) tree
												   in xs ++ (a, Branch op (snd y) (snd $ head ys)) : tail ys
								eval (Leaf x) = x
								eval (Branch s l r) | s == "+" = eval l + eval r
													| s == "-" = eval l - eval r
													| s == "*" = eval l * eval r
													| s == "/" = if eval r == 0 || eval l `rem` eval r /= 0
																 then -(eval l + 99 * (eval r)) - 999
																 else eval l `div` eval r
								str (Leaf x) = show x
								str (Branch s (Leaf l) (Leaf r)) = show l ++ s ++ show r
								str (Branch s l r) | s == "=" = str l ++ " = " ++ str r
												   | otherwise = "(" ++ str l ++ ") " ++ s ++ " (" ++ str r ++ ")"