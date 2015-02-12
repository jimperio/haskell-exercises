data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

fromConsList (Cons a (as)) = a:(fromConsList as)
fromConsList Nil           = []

check = fromConsList (fromList [1,2,3]) == [1,2,3]