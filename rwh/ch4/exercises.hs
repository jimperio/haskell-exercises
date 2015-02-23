import Interact

-- Exercise 1: safe list functions
safeHead :: [a] -> Maybe a
safeTail :: [a] -> Maybe [a]
safeLast :: [a] -> Maybe a
safeInit :: [a] -> Maybe [a]

safeHead = safeHelper head

safeTail = safeHelper tail

safeLast = safeHelper last

safeInit = safeHelper init

safeHelper :: ([t] -> a) -> [t] -> Maybe a
safeHelper f [] = Nothing
safeHelper f xs = Just (f xs)


-- Exercise 2: splitWith
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith p xs = splitWithHelper [] p xs

-- Initial attempt, could probably be made more concise.
-- Didn't use `takeWhile` or `dropWhile`.
splitWithHelper :: [[a]] -> (a -> Bool) -> [a] -> [[a]]
splitWithHelper res _ []     = res
splitWithHelper res p (x:xs) = splitWithHelper newRes p xs
  where newRes
          | null res || not (p x) = res ++ [[x]]
          | otherwise             = (init res) ++ [(last res) ++ [x]]

testPredicate :: Bool -> a -> Bool
testPredicate b _ = b

-- Exercise 3: Print first word of each input line.
firstWords :: String -> String
firstWords xs = unlines (map firstWord (lines xs))

firstWord :: String -> String
firstWord [] = []
firstWord xs = head (words xs)

--main = createMain firstWords

-- Exercise 4: Transpose text in a file: "hello\nworld\n" -> "hw\neo\nlr\nll\nod\n"
transpose :: String -> String
transpose [] = []
transpose xs = unlines (transposeHelper (lines xs))

transposeHelper :: [String] -> [String]
transposeHelper []     = []
transposeHelper (x:xs) = myZip (splitAll x) (transposeHelper xs)

splitAll = splitWith (testPredicate False)

myZip :: [String] -> [String] -> [String]
myZip as []         = as
myZip [] bs         = bs
myZip (a:as) (b:bs) = (a ++ b) : (myZip as bs)

--main = createMain transpose
