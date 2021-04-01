data T1 = A | B
data T2 = C | D

toInt :: Integral a => a -> Int
toInt x = fromIntegral x :: Int

{-
class CL a where
    func :: a -> Int

instance CL T1 where
    func A = 1
    func B = 2

instance CL T2 where
    func C = 1
    func D = 2
-}

main = return ()
