-- --------------------------------------------- --
-- -----------| Haskell Problem #21 -----------| --
-- --------------------------------------------- --

-- (*) Insert an element at a given position in a list
-- Prelude > insert e '(a b c d) 2
-- (A E B C D)

insert					:: a -> [a] -> Int -> [a]
insert x xs n			= take n xs ++ (x : drop n xs)