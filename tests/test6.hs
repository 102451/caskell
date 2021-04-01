
-- Test file 6: function tests
-- Note: the case statements may be optimized and these
-- tests may not actually test case statements in that
-- case.

-- f1 and f2 are isomorphic
f1 :: Int -> Int
f1 x = x + 1

f2 :: Int -> Int
f2 y = y + 1

f3 :: Int -> Int
f3 z = z

-- assert hash f1 == hash f2     isomorphic function definition = same hash
-- assert hash f1 /= hash f3     not isomorphic function definition = same hash

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
-- assert hash fl /= hash fr    different function argument = different hash
-- assert hash fl /= hash fl2   different argument type (different type signature) = different hash
-- assert hash fl /= hash fl3

-- fc1 and fc2 are isomorphic
-- fc1 and fc3 are not isomorphic
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

-- assert hash fc1 == hash fc2   isomorphic case statement = same hash
-- assert hash fc1 /= hash fc3   not isomorphic case statement = different hash

data MaybeIntPair = None | This Int Int

-- case binding variables
-- fcb1 and fcb2 are not isomorphic
fcb1 :: MaybeIntPair -> Int
fcb1 (This a b) = a
fcb1 None = 0

fcb2 :: MaybeIntPair -> Int
fcb2 (This a b) = b
fcb2 None = 0

-- assert hash fcb1 /= hash fcb2   different case var binding usage = different hash

-- let
-- flet1 and flet2 are not isomorphic
flet1 :: Int -> Int
flet1 x =
    let y = x + 1
    in y + y

flet2 :: Int -> Int
flet2 x =
    let y = 2 * x
    in y + y

-- assert hash flet1 /= hash flet2   different let definition = different hash

main = return ()
