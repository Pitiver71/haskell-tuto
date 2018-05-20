-- exercices from Chapter 2 of the book Real World Haskell

add :: Int -> Int -> Int
add a b = a + b

drop2 :: Int -> [a] -> [a]
drop2 n xs = if n <= 0 || null xs
    then xs
    else drop2 (n - 1) (tail xs)

drop3 :: Int -> [a] -> [a]
drop3 0 xs = xs
drop3 n xs = drop3 (n - 1) (tail xs)

isOdd :: Int -> Bool
isOdd n = mod n 2 == 1

lastButOne :: [a] -> a
lastButOne [x, y] = x
lastButOne xs = lastButOne (tail xs)