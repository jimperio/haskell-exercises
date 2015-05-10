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
localMaxima :: [Integer] -> [Integer]
localMaxima is = map getMax $ filter hasMax $ group is

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

-- Exercise 3: Histogram
-- histogram :: [Integer] -> String
