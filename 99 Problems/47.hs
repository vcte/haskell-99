-- --------------------------------------------- --
-- -----------| Haskell Problem #47 -----------| --
-- --------------------------------------------- --

-- (**) Define and, or, nand, nor, xor, impl, and equ, and table
-- Prelude > table2 (\a b -> (a `and'` (a `or'` b)))
-- True True True
-- True False True
-- False True False
-- False False False

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

table				:: (Bool -> Bool -> Bool) -> IO()
table f				= sequence_ $ map putStrLn
						[show a ++ " " ++ show b ++ " | " ++ show (f a b) 
						| a <- [True, False], b <- [True, False]]