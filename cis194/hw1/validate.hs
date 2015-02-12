-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits i
  | i <= 0    = []
  | otherwise =  toDigitsHelper [] i f
  where f ds l = l:ds

toDigitsRev :: Integer -> [Integer]
toDigitsRev i
  | i <= 0    = []
  | otherwise = toDigitsHelper [] i f
  where f ds l = ds ++ [l]

toDigitsHelper :: [Integer] -> Integer -> ([Integer] -> Integer -> [Integer]) -> [Integer]
toDigitsHelper ds i f
  | i == 0    = ds
  | otherwise = toDigitsHelper (f ds l) j f
  where l = i `mod` 10
        j = quot (i - l) 10


-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther is = fst (foldr doubleStep ([], 0) is)

doubleStep :: Integer -> ([Integer], Integer) -> ([Integer], Integer)
doubleStep n (is, l) = (n':is, l + 1)
  where n' = n * ((l `mod` 2) + 1)


-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits is = (foldr sumStep 0 is)

sumStep :: Integer -> Integer -> Integer
sumStep i s = s + sum (toDigits i)

-- Exercise 4
validate :: Integer -> Bool
validate i = sumDigits (doubleEveryOther (toDigits i)) `mod` 10 == 0