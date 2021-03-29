
-- function tests

-- f1 and f2 are isomorph
f1 :: Int -> Int
f1 x = x + 1

f2 :: Int -> Int
f2 y = y + 1

f3 :: Int -> Int
f3 z = z

-- fl and fr are not isomorph
-- fl and fl2 are not isomorph
-- fl and fl3 are not isomorph
fl :: Int -> Int -> Int
fl x y = x

fr :: Int -> Int -> Int
fr x y = y

fl2 :: Char -> Int -> Char
fl2 x y = x

fl3 :: Int -> Char -> Int
fl3 x y = x

-- fc1 and fc2 are isomorph
-- fc1 and fc3 are not isomorph
fc1 :: Bool -> Int
fc1 x = case x of
          True -> 1
          False -> 0

fc2 :: Bool -> Int
fc2 True = 1
fc2 False = 0

fc3 :: Bool -> Int
fc3 True = 1
fc3 _ = 0

main = return ()
