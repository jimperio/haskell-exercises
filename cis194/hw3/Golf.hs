-- Exercise 1: Hopscotch
skips :: [a] -> [[a]]
skips xs = map (every xs) [1..length xs]

every :: [a] -> Int -> [a]
-- Arguments flipped from natural order to save characters using `flip` above
every xs n = case drop (n - 1) xs of
               (y:ys) -> y : every ys n
               [] -> []

-- Exercise 2: Local maxima
-- Works, but feels like it could still be a lot more concise...
localMaximaOld :: [Integer] -> [Integer]
localMaximaOld is = map getMax $ filter hasMax $ group is

-- Generate all group of 3, e.g.:
-- [1,2,3,4,5] -> [[1,2,3],[2,3,4],[3,4,5]]
group :: [Integer] -> [[Integer]]
group [] = []
group s@(x:xs)
  | length s > 2 = (take 3 s) : group xs
  | otherwise    = []

-- Does a group contain a local maximum?
hasMax :: [Integer] -> Bool
hasMax (x:y:z:[])
  | y > x && y > z  = True
  | otherwise       = False
hasMax _            = False

-- Get the local maximum.
-- XXX: Unsafe!
getMax :: [Integer] -> Integer
getMax (_:y:_:[]) = y

-- Shorter and simpler attempt.
-- (Why did it ever seem more natural to generate groups??)
localMaxima :: [Integer] -> [Integer]
localMaxima is = h [] is

h :: [Integer] -> [Integer] -> [Integer]
h ms (i:j:k:is) = h ms' (j:k:is)
  where ms' = if j > i && j > k then ms ++ [j] else ms
h ms _     = ms

-- Exercise 3: Histogram
-- histogram :: [Integer] -> String
