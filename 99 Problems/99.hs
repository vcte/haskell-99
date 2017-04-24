-- --------------------------------------------- --
-- -----------| Haskell Problem #99 -----------| --
-- --------------------------------------------- --

-- (***) Crossword puzzle
-- crossword $ parse puzzle
--   A
--   L
-- POPPY ...

puzzle =	"ALPHA\n" ++ 
			"ARES\n" ++ 
			"POPPY\n\n" ++ 
			"  .  \n" ++ 
			"  .  \n" ++ 
			".....\n" ++ 
			"  . .\n" ++ 
			"  . .\n" ++ 
			"    .\n"
			
puzzle2 =	"PERL\n" ++ 
			"PROLOG\n" ++ 
			"ONLINE\n" ++ 
			"WEB\n" ++ 
			"GNU\n" ++ 
			"LINUX\n" ++ 
			"NFS\n" ++ 
			"SQL\n" ++ 
			"XML\n" ++ 
			"MAC\n" ++ 
			"EMACS\n\n" ++
			"......  .\n" ++ 
			". .  .  .\n" ++ 
			". ..... .\n" ++ 
			". . . ...\n" ++ 
			"  . ... .\n" ++ 
			" ...     \n"
			
parse					:: String -> ([String], [[Char]])
parse s					= let (xs, ys) = break (\(a, b) -> a == '\n' && b == '\n') $ zip s $ tail s
						  in (words $ map fst xs, lines $ map snd $ tail ys)
						  
crossword				:: ([String], [[Char]]) -> [[[Char]]]
crossword (ws, ps)		= crossword' (0, 0) (ws, ps)
						where
							crossword' pt@(x, y) sol | y == length ps = if null $ fst sol then [snd sol] else []
													 | True = [sol] >>= horiz pt >>= vert pt >>= crossword' (next pt)
							next (x, y) = let (d, m) = (x + 1) `divMod` (length (ps !! y))
										  in (m, y + d)
							horiz pt (ws, ps) = if ps !! (snd pt) !! (fst pt) == ' ' || (not $ any (hfit pt ps) ws) ||
												   (not $ hdot pt ps)
												then [(ws, ps)]
												else [([l | l <- ws, l /= w], hadd pt ps w) | w <- ws, hfit pt ps w]
							hdot (x, y) ps | (x >= length (ps !! y)) || (ps !! y !! x == ' ') = False
										   | otherwise = (ps !! y !! x == '.') || (hdot (x + 1, y) ps)
							hfit (x, y) ps []    = True
							hfit (x, y) ps (c:w) | y >= length ps || x >= length (ps !! y) = False
												 | otherwise = let p = ps !! y !! x
															   in (p == '.' || p == c) && hfit (x + 1, y) ps w
							hadd _      ps []    = ps
							hadd (x, y) ps (c:w) = hadd (x + 1, y) (add (x, y) ps c) w
							vert pt (ws, ps)  = if ps !! (snd pt) !! (fst pt) == ' ' || (not $ any (vfit pt ps) ws) ||
												   (not $ vdot pt ps)
												then [(ws, ps)]
												else [([l | l <- ws, l /= w], vadd pt ps w) | w <- ws, vfit pt ps w]
							vdot (x, y) ps | (y >= length ps) || (ps !! y !! x == ' ') = False
										   | otherwise = (ps !! y !! x == '.') || (vdot (x, y + 1) ps)
							vfit (x, y) ps []    = True
							vfit (x, y) ps (c:w) | y >= length ps || x >= length (ps !! y) = False
												 | otherwise = let p = ps !! y !! x
															   in (p == '.' || p == c) && vfit (x, y + 1) ps w
							vadd _      ps []    = ps
							vadd (x, y) ps (c:w) = vadd (x, y + 1) (add (x, y) ps c) w
							add (x, y) ps c = let l = ps !! y
											  in take y ps ++ [take x l ++ c : drop (x + 1) l] ++ drop (y + 1) ps