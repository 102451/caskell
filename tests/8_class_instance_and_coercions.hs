
-- test8: instance test, coercion test, cast test

data T1 = A | B
data T2 = C | D
data T3 = E Int

class CL a where
    func :: a -> Int

instance CL T1 where
    func A = 1
    func B = 2

instance CL T2 where
    func C = 1
    func D = 2

instance CL T3 where
    func (E i) = i

funcT1 :: T1 -> Int
funcT1 = func

funcT2 :: T2 -> Int
funcT2 = func

funcT3 :: T3 -> Int
funcT3 = func

-- assert hash funcT1 == hash funcT2    isomorphic instance definition of isomorphic type = same hash
-- assert hash funcT1 /= hash funcT3    not isomorphic instance definition of not isomorphic type = different hash

main = return ()
