-- Test file 1: number literal and var (reference) hash test
-- given a number literal 'a' = x
--   and a number literal 'b' = x
--   and a reference 'c' = 'a'
--
-- hash of a = hash of b = hash of c

-- Int = I# Int#
-- Int# = Primitive

-- assert hash a == hash b    same definition = same hash
a :: Int
a = 5 :: Int

b :: Int
b = 5

-- assert hash a == hash c    var to X = hash of X
c :: Int
c = a

-- assert hash c == hash d    var to var to X = hash of X
d = a

-- assert hash a /= hash e    different type = different hash
e :: Integer
e = 5

main = return ()
