

-- Test file 7: recursive function tests

-- f1 and f2 are not isomorphic
f1 :: [Int] -> Int
f1 [] = 0
f1 (_:h) = f1 h

f2 :: [Int] -> Int
f2 [] = 1
f2 (_:h) = f2 h
-- assert hash f1 /= hash f2   different direct recursion function definition = different hash

-- f3 and f4 are not isomorphic
-- f3 and f5 are isomorphic
-- f3 and f7 are not isomorphic
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
f7 (_:h) = f8 h

f8 :: [Int] -> Int
f8 [] = 42
f8 (_:h) = f7 h
-- assert hash f3 /= hash f4   different recursion order = different hash
-- assert hash f3 == hash f5   same indirect recursion definition = same hash
-- assert hash f4 == hash f6
-- assert hash f3 /= hash f7   different indirect recursion definition = same hash

-- recursive let
f9 :: Int -> Int
f9 x =
    let f9' a b = if a > 0 then f9' (a - 1) (b + x) else b
    in f9' x 0

f10 :: Int -> Int
f10 x =
    let f10' a b = if a > 0 then f10' (a - 1) (b + x) else b
    in f10' x 5
    
-- assert hash f9 /= hash f10

main = return ()
