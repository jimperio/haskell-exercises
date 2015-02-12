-- file: ch04/IntParse.hs
import Data.Char (digitToInt)

asInt :: String -> Int
loop :: Int -> String -> Int

asInt xs = loop 0 xs
loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs


asInt' xs = foldl step 0 xs

step :: Int -> Char -> Int
step acc x = acc * 10 + digitToInt x
