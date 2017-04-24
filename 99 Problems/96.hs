-- --------------------------------------------- --
-- -----------| Haskell Problem #96 -----------| --
-- --------------------------------------------- --

-- (**) Syntax checker
-- syntax "this-is-a-long-identifier"
-- True

syntax							:: String -> Bool
syntax s						= syntax' s 'l'
								where
									syntax' []    t = t == 'e'
									syntax' (c:s) t = (match c t) && or [syntax' s b | (a, b) <- fsm, a == t]
									fsm = [('l', 'e'), ('l', 'l'), ('l', '-'), ('l', 'd'), ('-', 'l'), 
										   ('-', 'd'), ('d', 'e'), ('d', '-'), ('d', 'l'), ('d', 'd')]
									match c t | t == '-' = c == '-'
											  | t == 'e' = c == ' '
											  | t == 'd' = '0' <= c && c <= '9'
											  | t == 'l' = 'a' <= c && c <= 'z'
														|| 'A' <= c && c <= 'Z'