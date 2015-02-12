length' :: [a] -> Int
length' []     = 0
length' [a]    = 1
length' (x:xs) = 1 + length' xs