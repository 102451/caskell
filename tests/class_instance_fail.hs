
-- class instance failing test:
-- cannot define different instances of the same class for isomorph types

data T1 = A | B
data T2 = C | D

class CL a where
    func :: a -> Int

instance CL T1 where
    func A = 1
    func B = 2

-- this is a problem, because now T1 and T2 are not isomorph anymore
instance CL T2 where
    func C = 3
    func D = 4

main = return ()
