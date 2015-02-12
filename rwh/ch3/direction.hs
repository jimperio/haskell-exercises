data Point = Point {
  x :: Double,
  y :: Double
} deriving (Show)

data Direction = DirLeft | DirRight | Straight
                 deriving (Show)

direction :: Point -> Point -> Point -> Direction
direction a b c
  | ccw > 0  = DirLeft
  | ccw == 0 = Straight
  | ccw < 0  = DirRight
  where ccw = (x b - x a)*(y c - y a) - (y b - y a)*(x c - x a)

-- Straight
a = Point 0 0
b = Point 1 1
c = Point 2 2

-- DirLeft
a1 = Point (-1) (-3)
b1 = Point 0 0
c1 = Point (-5) 10

-- DirRight
a2 = Point 6 5
b2 = Point 0 0
c2 = Point 6 6

{-- 
I'll leave the full implementation of the Graham scan algorithm
for another day. It seems like it would be a nice math-y exercise.
--}