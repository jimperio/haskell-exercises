lengthSort [] = []
lengthSort (p:xs) = (lengthSort lesser) ++ [p] ++ (lengthSort greater)
  where lesser = filter (shorterThan p) xs
        greater = filter (longerThan p) xs
        shorterThan p xs = length xs < length p
        longerThan p xs = length xs >= length p


sort' [] = []
sort' (p:xs) = (sort' lesser) ++ [p] ++ (sort' greater)
  where lesser = filter (< p) xs
        greater = filter (>= p) xs