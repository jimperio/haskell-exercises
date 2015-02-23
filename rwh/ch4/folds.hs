foldl :: (a -> b -> a) -> a -> [b] -> a

foldl step zero (x:xs) = foldl step (step zero x) xs
foldl _    zero []     = zero

--foldl (+) 0 (1:2:3:[])
--          == foldl (+) (0 + 1)             (2:3:[])
--          == foldl (+) ((0 + 1) + 2)       (3:[])
--          == foldl (+) (((0 + 1) + 2) + 3) []
--          ==           (((0 + 1) + 2) + 3)

niceSum :: [Integer] -> Integer
niceSum xs = foldl (+) 0 xs


foldr :: (a -> b -> b) -> b -> [a] -> b

foldr step zero (x:xs) = step x (foldr step zero xs)
foldr _    zero []     = zero

--foldr (+) 0 (1:2:3:[])
--          == 1 +           foldr (+) 0 (2:3:[])
--          == 1 + (2 +      foldr (+) 0 (3:[])
--          == 1 + (2 + (3 + foldr (+) 0 []))
--          == 1 + (2 + (3 + 0))

myFoldl :: (a -> b -> a) -> a -> [b] -> a

myFoldl f z xs = foldr step id xs z
    where step x g a = g (f a x)

--myFoldl (+) 0 [1, 2, 3]
--= (foldr step id [1, 2, 3]) 0
--= (step 1 (step 2 (step 3 id))) 0
--= (step 1 (step 2 (\a3 -> id ((+) a3 3)))) 0
--= (step 1 (\a2 -> (\a3 -> id ((+) a3 3)) ((+) a2 2))) 0
--= (\a1 -> (\a2 -> (\a3 -> id ((+) a3 3)) ((+) a2 2)) ((+) a1 1)) 0
--= (\a1 -> (\a2 -> (\a3 -> (+) a3 3) ((+) a2 2)) ((+) a1 1)) 0
--= (\a1 -> (\a2 -> (+) ((+) a2 2) 3) ((+) a1 1)) 0
--= (\a1 -> (+) ((+) ((+) a1 1) 2) 3) 0
--= (+) ((+) ((+) 0 1) 2) 3
--= ((0 + 1) + 2) + 3