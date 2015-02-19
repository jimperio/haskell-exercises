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