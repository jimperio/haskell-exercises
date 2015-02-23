-- 1: Write asInt (String -> Int) using a fold, 
-- handling certain error cases using `error`.
import Data.Char (digitToInt, isDigit, isSpace)
import Data.List (foldl')

asInt_fold :: String -> Int
asInt_fold []       = error "No empty strings, please."
asInt_fold "-"      = error "What does just a dash even mean?"
asInt_fold ('-':xs) = negate (asInt_fold xs)
asInt_fold xs       = foldl' step 0 xs
  where step acc x | not (isDigit x)        = error "Valid integer, please."
                   | div (10 * acc) 10 /= acc = error "Integer overflowed, sorry!"
                   | otherwise              = 10 * acc + digitToInt x

-- 2: Rewrite asInt_fold to use `Either` instead of `error`
type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either []       = Left "No empty strings, please."
asInt_either "-"      = Left "What does just a dash even mean?"
asInt_either ('-':xs) = either (\err -> Left err) (\res -> Right (negate res)) (asInt_either xs)
asInt_either xs       = foldl' step (Right 0) xs
  where step (Left err) x = Left err
        step (Right acc) x | not (isDigit x)          = Left "Valid integer, please."
                           | div (10 * acc) 10 /= acc = Left "Integer overflowed, sorry!"
                           | otherwise                = Right (10 * acc + digitToInt x)

-- 3: Define `concat` using `foldr`.
concat_foldr :: [[a]] -> [a]
concat_foldr = foldr (++) []

-- 4: Define `takeWhile` using a) explicit recursion, b) `foldr`.
takeWhile_recurse :: (a -> Bool) -> [a] -> [a]
takeWhile_recurse _ [] = []
takeWhile_recurse p (x:xs) | p x       = [x] ++ takeWhile_recurse p xs
                           | otherwise = []

takeWhile_foldr :: (a -> Bool) -> [a] -> [a]
takeWhile_foldr f xs = foldr step [] xs
  where step x acc | f x       = acc ++ [x]
                   | otherwise = []

-- 5: Define `Data.List.groupBy` using a fold.
f a b = a < b
groupBy_fold :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy_fold _ [] = []
groupBy_fold f (x:xs) = snd (foldl step (x, [[x]]) xs)
  where step (y, res) x | f y x     = (y, (init res) ++ [(last res) ++ [x]])
                        | otherwise = (x, res ++ [[x]])

-- 6: Define the ff. in terms of folds: a) `any`, `cycle`, `words`, `unlines`.
any_fold :: (a -> Bool) -> [a] -> Bool
any_fold _ [] = False
any_fold f xs = foldr step False xs
  where step x acc | acc || f x       = True
                   | otherwise = False

cycle_fold :: [a] -> [a]
cycle_fold [] = error "empty list"
cycle_fold xs = foldl step (cycle_fold xs) xs
  where step acc x = x : acc

words_fold :: String -> [String]
--words_fold xs = snd (foldl step (True, []) xs)
--  where step (s, res) x = (s', newRes)
--          where s' = isSpace x
--                newRes | s'        = res
--                       | s         = res ++ [[x]]
--                       | otherwise = (init res) ++ [(last res) ++ [x]]

-- To use (:) instead of ++, let's use `foldr` instead:
words_fold xs = snd (foldr step (True, []) xs)
  where step x (s, res) = (s', newRes)
          where s' = isSpace x
                newRes | s'        = res
                       | s         = [x] : res
                       | otherwise = (x : (head res)) : (tail res)

unlines_fold :: [String] -> String
unlines_fold ls = foldr step "" ls
  where step l acc = l ++ ('\n':acc)
