
-- test8: instance test, coercion test, cast test

data T1 = A | B
data T2 = C | D

class CL a where
    func :: a -> Int

instance CL T1 where
    func A = 1
    func B = 2

instance CL T2 where
    func C = 1
    func D = 2

main = return ()
