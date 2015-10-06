import Data.Array (Array)
import qualified Data.Array as Array
import Data.Char
import Data.List

-- Exercise 1: Hopscotch
skips :: [a] -> [[a]]
skips xs = map (every xs) [1..length xs]

every :: [a] -> Int -> [a]
-- Arguments flipped from natural order to save characters using `flip` above
every xs n = case drop (n - 1) xs of
               (y:ys) -> y : every ys n
               [] -> []

-- Exercise 2: Local maxima
-- Shorter and simpler attempt.
-- (Why did it ever seem more natural to generate groups??)
localMaxima :: [Integer] -> [Integer]
localMaxima is = h [] is

h :: [Integer] -> [Integer] -> [Integer]
h ms (i:j:k:is) = h ms' (j:k:is)
  where ms' = if j > i && j > k then ms ++ [j] else ms
h ms _     = ms

-- Works, but feels like it could still be a lot more concise...
localMaximaOld :: [Integer] -> [Integer]
localMaximaOld is = map getMax $ filter hasMax $ grp is

-- Generate all group of 3, e.g.:
-- [1,2,3,4,5] -> [[1,2,3],[2,3,4],[3,4,5]]
grp :: [Integer] -> [[Integer]]
grp [] = []
grp s@(x:xs)
  | length s > 2 = (take 3 s) : grp xs
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
-- * Start with an array containing strings of the format
--   "0=", "1=", .., "9=".
-- * Consume the list, adding '*' to the corresponding string
--   and keeping track of the maximum length.
-- * Convert from horizontal to vertical histogram by:
--   * Padding the strings with spaces to the maximum length.
--   * Transpose, reverse, then join the strings.
histogram :: [Integer] -> String
histogram is = unlines $ reverse $ transpose (f n 0 is)
  where n    = Array.array (0, 9) [(i, (intToDigit i) : "=") | i <- [0..9]]

-- This doesn't handle unexpected inputs, e.g., integers not in [0..9].
f :: Array Int String -> Int -> [Integer] -> [String]
f cs m (i:is) = f cs' m' is
  where cs'   = cs Array.// [(j, cs Array.! j ++ "*")]
        j     = fromIntegral i
        m'    = max m n
        n     = length (cs' Array.! j)
f cs m []     = map (p m) (Array.elems cs)

p :: Int -> String -> String
p m s
  | length s < m = p m (s ++ " ")
  | otherwise    = s
