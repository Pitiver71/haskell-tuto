count :: [a] -> Int
count [] = 0
count (x:xs) = 1 + count xs

mean :: (Fractional a) => [a] -> a
mean [] = 0
mean xs = loop 0 0 xs
    where loop l acc [] = acc / fromIntegral l
          loop l acc (x:xs) = loop (l + 1) (acc + x) xs