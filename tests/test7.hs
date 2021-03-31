

-- recursive function tests

-- f1 and f2 are not isomorph
f1 :: [Int] -> Int
f1 [] = 0
f1 (_:h) = f1 h

f2 :: [Int] -> Int
f2 [] = 1
f2 (_:h) = f2 h

-- f3 and f4 are not isomorph
-- f3 and f5 are isomorph
-- f3 and f7 are not isomorph
f3 :: [Int] -> Int
f3 [] = 0
f3 (_:h) = f4 h

f4 :: [Int] -> Int
f4 [] = 1
f4 (_:h) = f3 h

f5 :: [Int] -> Int
f5 [] = 0
f5 (_:h) = f6 h

f6 :: [Int] -> Int
f6 [] = 1
f6 (_:h) = f5 h

f7 :: [Int] -> Int
f7 [] = 0
f7 (_:h) = f6 h

f8 :: [Int] -> Int
f8 [] = 42
f8 (_:h) = f5 h

-- recursive let
f9 :: Int -> Int
f9 x =
    let f a b = if a > 0 then f (a - 1) (b + x) else b
    in f x 0

f10 :: Int -> Int
f10 x =
    let f a b = if a > 0 then f (a - 1) (b + x) else b
    in f x 5
    

main = return ()
