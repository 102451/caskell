-- Test file 2: literal test
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
-- assert hash int5 == hash int5_2   same int definition = same hash
int5_2 = 5

-- assert hash int5 /= hash int3     different int definition = different hash
int3 = 3

float3'14 :: Float
float3'14 = 3.14

-- assert hash float3'14 == hash float3'14_2   same float definition = same hash
float3'14_2 :: Float
float3'14_2 = 3.14

-- assert hash float3'14 /= hash float2'71     different float definition = different hash
float2'71 :: Float
float2'71 = 2.71

double3'14 :: Double
double3'14 = 3.14

-- assert hash double3'14 == hash double3'14_2   same double definition = same hash
double3'14_2 :: Double
double3'14_2 = 3.14

-- assert hash double3'14 /= hash double2'71     different double definition = different hash
double2'71 :: Double
double2'71 = 2.71

charA :: Char
charA = 'A'

-- assert hash charA == hash charA_2   same char definition = same hash
charA_2 :: Char
charA_2 = 'A'

-- assert hash charA /= hash charB     different char definition = different hash
charB :: Char
charB = 'B'

stringHello :: String
stringHello = "Hello"

-- assert hash stringHello == hash stringHello_2   same string definition = same hash
stringHello_2 :: String
stringHello_2 = "Hello"

-- assert hash stringHello /= hash stringWorld     different string definition = different hash
stringWorld :: String
stringWorld = "World"

main :: IO ()
main = return ()
