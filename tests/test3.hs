-- test3 simple types (data)
-- T1 = T2
-- T1 =/= T3
-- T2 =/= T3

data T1 = A | B
data T2 = C | D
data T3 = E | F | G

--i = 5

{-
data T1 = X
data T2 = Y

class ClassA a where
    funcA :: a -> Int

instance ClassA T1 where
    funcA X = 5

instance ClassA T2 where
    funcA Y = 10
-}

main :: IO ()
main = undefined
