data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)
leftTree = Node "grandparent" simpleTree Empty
rightTree = Node "grandparent" Empty simpleTree
deepTree = Node "ancestor" (Node "hello" rightTree leftTree) Empty

-- Single constructor using Maybe
data Tree' a = Node' a (Maybe (Tree' a)) (Maybe (Tree' a))
               deriving (Show)

tree = Node' "parent" (Just (Node' "left child" Nothing Nothing)) (Just (Node' "right child" Nothing Nothing))

-- The above doesn't allow for an empty tree! Let's fix it.
data RightTree a = RightTree (Maybe (a, RightTree a, RightTree a)) deriving (Show)
empty = RightTree Nothing
realTree = RightTree (Just ("parent", empty, empty))
deeperTree = RightTree (Just ("grandparent", realTree, realTree))
deepestTree = RightTree (Just ("ancestor", (RightTree (Just ("descendant", deeperTree, empty))), empty))


-- Exercise: determine height of a tree.
height :: Tree a -> Int
height Empty                = 0
height (Node _ l r)         = 1 + max (height l) (height r)

height' :: Maybe(Tree' a) -> Int
height' Nothing = 0
height' (Just (Node' _ l r)) = 1 + max (height' l) (height' r)

rightHeight :: RightTree a -> Int
rightHeight (RightTree Nothing)      = 0
rightHeight (RightTree (Just (_, l, r))) = 1 + max (rightHeight l) (rightHeight r)