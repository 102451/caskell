
-- Test file 10: typeclass constraint

f1 :: Show a => a -> String
f1 x = "abc"

f2 :: Show a => a -> String
f2 y = "abc"

f3 :: a -> String
f3 z = "abc"

-- assert hash f1 == hash f2    same typeclass contraint set = same hash
-- assert hash f1 /= hash f3    different typeclass contraint set = different hash

--data Eq x => T1 x = A x | B
--data Eq y => T2 y = C | D y
--data T3 z = E z | F

-- #assert hash T1 == hash T2    same stupid theta type set = same hash
-- #assert hash T1 /= hash T3    different stupid theta type set = different hash

main = return ()
