{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches c g = length (filter isMatch (zip c g))
  where isMatch x = fst x == snd x

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors c = map countColor colors
  where countColor y = length (filter (==y) c)

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches c g = sum (map getMin (zip (countColors c) (countColors g)))
  where getMin x = min (fst x) (snd x)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove c g = Move g exact nonExact
  where exact    = exactMatches c g
        nonExact = matches c g - exact

-- Exercise 4 -----------------------------------------
code :: Move -> Code
code  (Move c _ _) = c

isConsistent :: Move -> Code -> Bool
isConsistent m c = m == (getMove c (code m))

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m gs = filter (isConsistent m) gs

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = map (\c -> [c]) colors
allCodes n = allCodesHelper (allCodes (n - 1))

allCodesHelper :: [Code] -> [Code]
allCodesHelper gs = concatMap addColors gs
  where addColors g  = map (\c -> c:g) colors

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve c = solveHelper c [getMove c firstCode] remainingCodes
  where firstCode      = take (length c) (repeat Red)
        remainingCodes = filter (/= firstCode) (allCodes (length c))

solveHelper :: Code -> [Move] -> [Code] -> [Move]
solveHelper c ms rcs
  | code (last ms) == c = ms
  | otherwise           = solveHelper c (ms ++ [nextMove]) filteredCodes
  where nextMove      = getMove c (head rcs)
        filteredCodes = filterCodes nextMove (tail rcs)

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined