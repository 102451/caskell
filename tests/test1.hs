-- test 1: number literal and var (reference) hash test
-- given a number literal 'a' = x
--   and a number literal 'b' = x
--   and a reference 'c' = 'a'
--
-- hash of a = hash of b = hash of c

-- Int = I# Int#
-- Int# = Primitive
a :: Int
a = 5 :: Int

b :: Int
b = 5

c :: Int
c = a

d = a

e :: Integer
e = 5

main :: IO ()
main = return ()
