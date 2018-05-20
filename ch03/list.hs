data List a = Cons a (List a)
            | Nil
              deriving (Show)

data Tree a = Leaf a (Tree a) (Tree a)
            | Empty
              deriving (Show)


data Tree2 a = Leaf2 a (Maybe a) (Maybe a) deriving (Show)

myList = Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 (Cons 0 Nil)))))

toList :: [a] -> List a
toList (x:xs) = Cons x (toList xs)
toList [] = Nil

fromList :: List a -> [a]
fromList (Cons x (Nil)) = [x]
fromList (Cons x (y)) = [x] ++ (fromList y)
