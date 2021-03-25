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
-- T7 and T8 are isomorph
data T7  = O (Int -> Char)
data T8  = P (Int -> Char)
data T9  = Q (Char -> Int)
data T10 = R (Int -> Int -> Char)

-- type arguments
-- T11 and T12 are isomorph, T13 and T14 are isomorph
data T11 a = S a | T
data T12 b = U | V b
data T13 a b = W a b | X b a
data T14 a b = Y b a | Z a b

-- T13' and T14' are NOT isomorph, order of arguments matters
data T13' a b = W' a b
data T14' a b = Y' b a

-- tuples
-- T15 and T16 are isomorph
data T15 = AA (Int, Char)
data T16 = AB (Int, Char)
data T17 = AC (Char, Int)

-- record types
-- T18 and T19 are isomorph
data T18 = AD { ad_a :: Int, ad_b :: Char }
data T19 = AE { ae_a :: Int, ae_b :: Char }
data T20 = AF { af_a :: Char, af_b :: Int } -- order matters


main :: IO ()
main = return ()
