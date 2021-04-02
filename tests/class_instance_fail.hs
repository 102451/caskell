
-- class instance failing test:
-- cannot define different instances of the same class for isomorphic types

data T1 = A | B
data T2 = C | D

class CL a where
    func :: a -> Int

instance CL T1 where
    func A = 1
    func B = 2

instance CL T2 where
    func C = 3
    func D = 4

funcT1 :: T1 -> Int
funcT1 = func

funcT2 :: T2 -> Int
funcT2 = func

-- this is a problem, because now T1 and T2 cant be interchanged anymore
-- even though they are isomorphic, because "func t" produces a different
-- output depending on the type of t.
-- assert hash funcT1 /= hash funcT2

main = return ()
