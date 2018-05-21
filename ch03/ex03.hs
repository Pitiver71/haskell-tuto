import Data.List

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