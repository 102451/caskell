-- test4 recursive data types

data MyList = EmptyList | Elem Int MyList
data T1 = X | Y Int T1
data T2 = A | B Float T3
data T3 = C | D Int T2

-- these are isomorph
data T4 = E | F Char T5
data T5 = G | H Char T6
data T6 = I Char T4 | J

-- T7 and T9 are NOT isomorph
data T7 = K | L Int T8
data T8 = M (Int -> T7)

data T9  = N | O Int T10
data T10 = P (Float, T9)

main :: IO ()
main = return ()
