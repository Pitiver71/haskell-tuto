import Data.List

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

simpleTree = Node "parent" (Node "left child" Empty Empty) (Node "right child" Empty Empty)              
bigTree = Node "root" 
            (Node "left 1" 
                (Node "left 12" 
                    (Node "left 121" Empty Empty)
                    Empty
                )
                (Node "right 12" Empty Empty)
            )
            (Node "right 2" 
                (Node "left 21" Empty Empty)
                (Node "right 22" 
                    (Node "left 221" 
                        (Node "left 2211" Empty Empty)
                        (Node "right 2212" Empty Empty)
                    )
                    (Node "right 222" Empty Empty)
                )
            )

data Point = Point {
    x :: Int,
    y :: Int
} deriving (Show)

data Direction = LEFT
               | RIGHT
               | STRAIGHT
               deriving (Show)

count :: [a] -> Int
count [] = 0
count (x:xs) = 1 + count xs

mean :: Fractional a => [a] -> a
mean [] = 0
mean xs = loop 0 0 xs
    where loop lgt acc [] = acc / fromIntegral lgt
          loop lgt acc (x:xs) = loop (lgt + 1) (acc + x) xs

palindrome :: [a] -> [a]
palindrome [] = []
palindrome xs = xs ++ (rev xs)
    where rev [] = []
          rev xs = [last xs] ++ rev (init xs)

palindrome2 :: [a] -> [a]
palindrome2 [] = []
palindrome2 xs = xs ++ reverse xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = False
isPalindrome [x] = True
isPalindrome [x, y] = x == y
isPalindrome xs = ((head xs) == (last xs)) && isPalindrome (drop 1 (init xs))

sortListOfLists :: Eq a => [[a]] -> [[a]]
sortListOfLists [] = []
sortListOfLists xss = sortBy (\xs ys -> compare (length xs) (length ys)) xss

intersperce :: a ->[[a]] -> [a]
intersperce _ [] = []
intersperce _ [x] = x
intersperce s (x:xs) = x ++ [s] ++ (intersperce s xs)

height :: Tree a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)