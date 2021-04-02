-- Test file 1: number literal and var (reference) hash test
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
-- assert hash a == hash b    same definition = same hash

c :: Int
c = a

d = a
-- assert hash a == hash c    var to X = hash of X
-- assert hash c == hash d    var to var to X = hash of X

e :: Integer
e = 5
-- assert hash a /= hash e    different type = different hash

main = return ()
