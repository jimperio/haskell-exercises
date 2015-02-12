palindrome [] = []
palindrome (x:xs) = x:(palindrome xs) ++ [x]

isPalindrome [] = True
isPalindrome xs = rev' xs == xs

rev' [] = []
rev' [x] = [x]
rev' (x:xs) = rev' xs ++ [x]