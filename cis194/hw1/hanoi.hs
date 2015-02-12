-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

hanoi n a b c
  | n == 1    = [(a, b)]
  | otherwise = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

-- Exercise 6
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
  | n == 0    = []
  | n == 1    = [(a, b)]
  -- Note that the order of pegs to be used as temporary storage is important here!
  | otherwise = hanoi4 y a d b c ++ hanoi4 x a c d b ++ [(a, b)] ++ hanoi4 x c b d a ++ hanoi4 y d b a c
  where x = quot (n - 1) 2
        y = n - x - 1
