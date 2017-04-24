-- --------------------------------------------- --
-- -----------| Haskell Problem #95 -----------| --
-- --------------------------------------------- --

-- (**) English number words
-- fullWords 175
-- one-seven-five

fullWords						:: Int -> String
fullWords n						| n < 10 = conv n
								| otherwise = fullWords (n `div` 10) ++ "-" ++ conv (n `mod` 10)
									where
										conv n
											| n == 0 = "zero"
											| n == 1 = "one"
											| n == 2 = "two"
											| n == 3 = "three"
											| n == 4 = "four"
											| n == 5 = "five"
											| n == 6 = "six"
											| n == 7 = "seven"
											| n == 8 = "eight"
											| n == 9 = "nine"
											| otherwise = ""