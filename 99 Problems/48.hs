-- --------------------------------------------- --
-- -----------| Haskell Problem #48 -----------| --
-- --------------------------------------------- --

-- (**) Truth tables for logical expressions
-- Prelude > tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
-- True  True  True  True
-- True  True  False True
-- True  False True  True
-- True  False False True
-- False True  True  True
-- False True  False True
-- False False True  True
-- False False False True

and'				:: Bool -> Bool -> Bool
and' True True		= True
and' _ _			= False
infixl 6 `and'`

or'					:: Bool -> Bool -> Bool
or'	False False		= False
or' _ _				= True
infixl 4 `or'`

nand'				:: Bool -> Bool -> Bool
nand' a b			= not $ and' a b

nor'				:: Bool -> Bool -> Bool
nor' a b			= not $ or' a b

xor'				:: Bool -> Bool -> Bool
xor' a b			= not $ equ' a b

impl'				:: Bool -> Bool -> Bool
impl' True False	= False
impl' _ _			= True

equ'				:: Bool -> Bool -> Bool
equ'				= (==)
infixl 3 `equ'`

table				:: (Bool -> Bool -> Bool) -> IO()
table f				= sequence_ $ map putStrLn
						[show a ++ " " ++ show b ++ " | " ++ show (f a b) 
						| a <- [True, False], b <- [True, False]]
						
tablen				:: Int -> ([Bool] -> Bool) -> IO()
tablen n f			= sequence_ $ map putStrLn
						[show a ++ " : " ++ show (f a)
						| a <- args n]
						where
							args 1 = [[True], [False]]
							args n = concat [map (a:) $ args (n - 1) | a <- [True, False]]