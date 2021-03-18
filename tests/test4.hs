-- test4 recursive data types

data MyList = EmptyList | Elem Int MyList
data T1 = X | Y Int T1
data T2 = A | B Float T3
data T3 = C | D Int T2


data T4 = E | F Char T5
data T5 = G | H Char T4

main :: IO ()
main = return ()
