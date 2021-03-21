
data T = A Int | B U U
data U = C Char | D T

data T1 = X Int | L [T1] | Y T1 T1 T1 | Z (T1, [Char]) | Omega (T1, [(T, U, (T1, T1))])

main = undefined
