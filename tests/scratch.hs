f9 :: Int -> Int
f9 x =
    let f a b = if a > 0 then f (a - 1) (b + x) else b
    in f x 0

main = undefined
