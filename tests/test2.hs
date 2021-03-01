-- test 2: literal test
-- given a literal 'a' = x
--   and a literal 'b' = x
--   and a literal 'c' = y
--   where y =/= x
--
-- hash of a = hash of b
-- hash of a =/= hash of c
-- hash of b =/= hash of c

-- Int = I# Int#
-- Int# = Primitive
int5 = 5
int5_2 = 5

int3 = 3

float3'14 :: Float
float3'14 = 3.14

float3'14_2 :: Float
float3'14_2 = 3.14

float2'71 :: Float
float2'71 = 2.71

double3'14 :: Double
double3'14 = 3.14

double3'14_2 :: Double
double3'14_2 = 3.14

double2'71 :: Double
double2'71 = 2.71

charA :: Char
charA = 'A'

charA_2 :: Char
charA_2 = 'A'

charB :: Char
charB = 'B'

stringHello :: String
stringHello = "Hello"

stringHello_2 :: String
stringHello_2 = "Hello"

stringWorld :: String
stringWorld = "World"

main :: IO ()
main = undefined
