-- file: ex01.hs
-- lines beginning with "--" are comments.

main = interact wordCount
    where wordCount input = show (length (input)) ++ "\n"
