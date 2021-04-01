
-- Test file 5: typeclasses

-- class C1 and C2 are isomorph
class C1 a where
    c_func :: a -> Int

class C2 x where
    otherFunc :: x -> Int

class C3 a where
    f :: a -> Int
    g :: a -> Int

-- assert hash C1 == hash C2   isomorphic class definitions = same hash
-- assert hash C1 /= hash C3   not isomorphic class definitions = different hash

main = return ()

