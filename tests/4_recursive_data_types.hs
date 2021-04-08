-- Test file 4: recursive data types

data MyList = EmptyList | Elem Int MyList
data T1 = X | Y Int T1
data T2 = A | B Float T3
data T3 = C | D Int T2
-- assert hash MyList == hash T1    same direct recursive type definition = same hash
-- assert hash T1 /= hash T2        different direct recursive type definition = different hash
-- assert hash T1 /= hash T3        different indirect recursive type definition = different hash
-- assert hash X /= hash A
-- assert hash X /= hash C
-- assert hash A /= hash C
-- assert hash Y /= hash B
-- assert hash Y /= hash D
-- assert hash B /= hash D

-- these are isomorphic
data T4 = E | F Char T5
data T5 = G | H Char T6
data T6 = I Char T4 | J
-- assert hash T4 == hash T5   same indirect recursive type definition = same hash
-- assert hash T5 == hash T6
-- assert hash T6 == hash T4
-- assert hash E == hash G
-- assert hash E == hash J
-- assert hash G == hash J
-- assert hash F == hash H
-- assert hash F == hash I
-- assert hash H == hash I

-- T7 and T9 are NOT isomorphic
data T7 = K | L Int T8
data T8 = M (Int -> T7)

data T9  = N | O Int T10
data T10 = P (Float, T9)
-- assert hash T7 /= hash T9    different indirect recursive function and tuple type arguments = different hash

main :: IO ()
main = return ()
