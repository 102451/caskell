-- Test file 3: simple data types

-- T1 and T2 are equal in content, and as such interchangeable (isomorphic)
data T1 = A | B
data T2 = C | D
data T3 = E | F | G
-- assert hash T1 == hash T2    same set of datacons = same hash
-- assert hash T1 /= hash T3    different number of datacons = different hash
-- assert hash A /= hash B      different datacon of same type = different hash (position dependent)
-- assert hash A == hash C      isomorphic datacon of isomorphic types = same hash
-- assert hash B == hash D
-- assert hash A /= hash E
-- assert hash A /= hash F
-- assert hash A /= hash G
-- assert hash B /= hash E
-- assert hash B /= hash F
-- assert hash B /= hash G

-- T4 and T5 are isomorphic
data T4 = H Int | I
data T5 = J | K Int
data T6 = L Int | M Int
-- assert hash T4 == hash T5    order of datacons does not alter hash
-- assert hash T4 /= hash T6
-- assert hash H == hash K
-- assert hash I == hash J
-- assert hash H /= hash L
-- assert hash H /= hash M

-- function data constructor arguments 
-- T7 and T8 are isomorphic
data T7  = O (Int -> Char)
data T8  = P (Int -> Char)
data T9  = Q (Char -> Int)
data T10 = R (Int -> Int -> Char)
-- assert hash T7 == hash T8    same function type argument = same hash
-- assert hash T7 /= hash T9    different function type argument = different hash
-- assert hash T7 /= hash T10   different function type argument = different hash
-- assert hash O == hash P
-- assert hash O /= hash Q
-- assert hash O /= hash R

-- type arguments
-- T11 and T12 are isomorph, T13 and T14 are isomorphic
data T11 a = S a | T
data T12 b = U | V b
data T13 a b = W a b | X b a
data T14 a b = Y b a | Z a b
-- assert hash T11 == hash T12
-- assert hash T11 /= hash T13
-- assert hash T13 == hash T14
-- assert hash T12 /= hash T14
-- assert hash S == hash V    same type arguments = same hash
-- assert hash T == hash U
-- assert hash W == hash Z
-- assert hash W /= hash Y
-- assert hash Y == hash X
-- assert hash Z /= hash X

-- T13' and T14' are NOT isomorphic, order of arguments matters
data T13' a b = W' a b
data T14' a b = Y' b a
-- assert hash T13' /= hash T14'   different order of type arguments = different hash
-- assert hash W' /= hash Y'   different order of type arguments = different hash

-- tuples
-- T15 and T16 are isomorphic
data T15 = AA (Int, Char)
data T16 = AB (Int, Char)
data T17 = AC (Char, Int)
-- assert hash T15 == hash T16     same tuple type argument = same hash
-- assert hash T15 /= hash T17     different tuple type argument = different hash
-- assert hash AA == hash AB
-- assert hash AA /= hash AC

-- record types
-- T18 and T19 are isomorphic
data T18 = AD { ad_a :: Int, ad_b :: Char }
data T19 = AE { ae_a :: Int, ae_b :: Char }
data T20 = AF { af_a :: Char, af_b :: Int } -- order matters
-- assert hash T18 == hash T19      same record constructor == same hash
-- assert hash T18 /= hash T20      different order of record fields == different hash
-- assert hash AD == hash AE
-- assert hash AD /= hash AF

main :: IO ()
main = return ()
