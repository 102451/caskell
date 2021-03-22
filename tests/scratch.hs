
--data T = A Int | B U U
--data U = C Char | D T

-- data T1 = X Int | L [T1] | Y T1 T1 T1 | Z (T1, [Char]) | Omega (T1, [(T, U, (T1, T1))])
--data T1 = A | C Int Int | X Int | Y T2 | B
--data T2 = Z | W Int T2

data T1 = A (Int -> T2) | B (T2 -> T1 -> T2 -> T1) | C T2 T1 T2 T1
data T2 = X | Y

main = undefined
