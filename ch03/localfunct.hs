fact1 :: Int -> Int
fact1 0 = 0
fact1 1 = 1
fact1 n = n * fact1 (n-1)


fact2 :: Int -> Int
fact2 0 = 0
fact2 1 = 1
fact2 n = loop 1 n
    where loop acc 0 = acc
          loop acc n = loop (acc * n) (n - 1)