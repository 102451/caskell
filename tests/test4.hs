-- test4 recursive data types

data MyList = EmptyList | Elem Int MyList
data T1 = X | Y Int T1
data T2 = A | B Float T3
data T3 = C | D Int T2

-- these are isomorph
data T4 = E | F Char T5
data T5 = G | H Char T6
data T6 = I Char T4 | J

main :: IO ()
main = return ()
