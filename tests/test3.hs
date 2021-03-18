-- test3 simple types (data)

-- T1 and T2 are equal in content, and as such interchangeable (isomorph)
data T1 = A | B
data T2 = C | D
data T3 = E | F | G

-- T4 and T5 are isomorph
data T4 = H Int | I
data T5 = J | K Int
data T6 = L Int | M Int

-- function data constructor arguments 
data T7  = O (Int -> Char)
data T8  = P (Int -> Char)
data T9  = Q (Char -> Int)
data T10 = R (Int -> Int -> Char)

-- type arguments
data T11 a = S a | T
data T12 b = U | V b
data T13 a b = W a b | X b a
data T14 a b = Y b a | Z a b

main :: IO ()
main = return ()
