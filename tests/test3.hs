-- test3 simple types (data)

-- T1 and T2 are equal in content, and such interchangeable (isomorph)
data T1 = A | B
data T2 = C | D
data T3 = E | F | G

-- T4 and T5 are isomorph
data T4 = H Int | I
data T5 = J | K Int
data T6 = L Int | M Int

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
