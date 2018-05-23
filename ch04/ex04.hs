import Data.List

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just $ init xs

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs = loop [] [] xs
    where loop curr acc [] = acc ++ [curr]
          loop curr acc (x:xs) | not $ p x = loop (curr ++ [x]) acc xs
                               | p x = loop [x] (newacc curr acc) xs
                                    where newacc [] acc = acc
                                          newacc curr acc = acc ++ [curr]

firstWords :: String -> IO()
firstWords str = printall $ map (\xs -> head xs) $ map words $ lines str
                    where printall [] = return ()
                          printall (x:xs) = putStrLn x >> printall xs