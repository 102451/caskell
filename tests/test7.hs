

-- recursive function tests

-- f1 and f2 are not isomorph
f1 :: [Int] -> Int
f1 [] = 0
f1 (_:h) = f1 h

f2 :: [Int] -> Int
f2 [] = 1
f2 (_:h) = f2 h


main = return ()
