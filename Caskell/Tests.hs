
module Caskell.Tests
(
    run_tests,
    test1,
    test2,
    test3,
    test4,
    test5,
    run_tests',

    scratch,
    scratch'
) where

import Unique
import Data.Maybe
import Data.Map.MultiKey
import Control.Monad
import Control.Exception
import Caskell.Context
import Caskell.Compile
import Caskell.DepGraph

debug_output = False

-- utils
get_tyDepGraph :: UniqueHash -> Maybe TyDepGraph
get_tyDepGraph uh = do
    defref <- unique_definition_ref uh
    let hd = hash_data defref
    case hd of
      TyCon tc -> Just $ dep_graph_from_tyCon tc
      _ -> Nothing


test_header :: Int -> String -> IO ()
test_header num description = do
    putStrLn $ "\ntest " ++ show num ++ ": " ++ description

init_test :: Int -> String -> IO (Context)
init_test num description = do
    test_header num description
    compile_file ("tests/test" ++ show num ++ ".hs") debug_output
    
assertEqual :: Eq a => String -> a -> a -> IO ()
assertEqual assertion x y = unless (x == y) (error $ "assertion failed: " ++ assertion)

assertNotEqual :: Eq a => String -> a -> a -> IO ()
assertNotEqual assertion x y = when (x == y) (error $ "assertion failed: " ++ assertion)

pass :: IO ()
pass = putStrLn "passed"

-- error when not found
get_hashed_expr' :: String -> Context -> UniqueHash
get_hashed_expr' var ctx = fromJust $ lookup_name var ctx

test1 :: IO ()
test1 = do
    ctx <- init_test 1 "number literal and reference equality"
    let get_hashed_expr = flip (get_hashed_expr') ctx

    let a = get_hashed_expr "a"
    let b = get_hashed_expr "b"
    let c = get_hashed_expr "c"
    let d = get_hashed_expr "d"
    let e = get_hashed_expr "e"

    assertEqual "hash a == hash b" (hash a) (hash b)
    assertEqual "hash a == hash c" (hash a) (hash c)
    assertEqual "hash a == hash d" (hash a) (hash d)
    assertEqual "hash b == hash c" (hash b) (hash c)
    assertEqual "hash b == hash d" (hash b) (hash d)
    assertEqual "hash c == hash d" (hash c) (hash d)

    assertNotEqual "hash a /= hash e" (hash a) (hash e)

    pass

test2 :: IO ()
test2 = do
    ctx <- init_test 2 "literals test"
    let get_hashed_expr = flip (get_hashed_expr') ctx

    let int5   = get_hashed_expr "int5"
    let int5_2 = get_hashed_expr "int5_2"
    let int3   = get_hashed_expr "int3"

    assertEqual "hash int5 == hash int5_2" (hash int5) (hash int5_2)
    assertNotEqual "hash int5 /= hash int3" (hash int5) (hash int3)

    let float3'14   = get_hashed_expr "float3'14"
    let float3'14_2 = get_hashed_expr "float3'14_2"
    let float2'71   = get_hashed_expr "float2'71"

    assertEqual "hash float3'14 == hash float3'14_2" (hash float3'14) (hash float3'14_2)
    assertNotEqual "hash float3'14 /= hash float2'71" (hash float3'14) (hash float2'71)

    let double3'14   = get_hashed_expr "double3'14"
    let double3'14_2 = get_hashed_expr "double3'14_2"
    let double2'71   = get_hashed_expr "double2'71"

    assertEqual "hash double3'14 == hash double3'14_2" (hash double3'14) (hash double3'14_2)
    assertNotEqual "hash double3'14 /= hash double2'71" (hash double3'14) (hash double2'71)

    let charA   = get_hashed_expr "charA"
    let charA_2 = get_hashed_expr "charA_2"
    let charB   = get_hashed_expr "charB"

    assertEqual "hash charA == hash charA_2" (hash charA) (hash charA_2)
    assertNotEqual "hash charA /= hash charB" (hash charA) (hash charB)

    let stringHello   = get_hashed_expr "stringHello"
    let stringHello_2 = get_hashed_expr "stringHello_2"
    let stringWorld   = get_hashed_expr "stringWorld"

    assertEqual "hash stringHello == hash stringHello_2" (hash stringHello) (hash stringHello_2)
    assertNotEqual "hash stringHello /= hash stringWorld" (hash stringHello) (hash stringWorld)

    pass

test3 :: IO ()
test3 = do
    ctx <- init_test 3 "simple data types"
    let get_hashed_expr = flip (get_hashed_expr') ctx

    let t1 = get_hashed_expr "T1"
    let t2 = get_hashed_expr "T2"
    let t3 = get_hashed_expr "T3"

    assertEqual "hash T1 == hash T2" (hash t1) (hash t2)
    assertNotEqual "hash T1 /= hash T3" (hash t1) (hash t3)

    let t4 = get_hashed_expr "T4"
    let t5 = get_hashed_expr "T5"
    let t6 = get_hashed_expr "T6"

    assertEqual "hash T4 == hash T5" (hash t4) (hash t5)
    assertNotEqual "hash T4 /= hash T6" (hash t4) (hash t6)

    -- let a = get_hashed_expr "A"
    let a = get_hashed_expr "A"
    let b = get_hashed_expr "B"
    let c = get_hashed_expr "C"
    let d = get_hashed_expr "D"
    let e = get_hashed_expr "E"

    assertNotEqual "hash A /= hash B" (hash a) (hash b)
    assertNotEqual "hash C /= hash D" (hash c) (hash d)
    assertEqual "hash A == hash C" (hash a) (hash c)
    assertEqual "hash B == hash D" (hash b) (hash d)

    let h = get_hashed_expr "H"
    let i = get_hashed_expr "I"
    let j = get_hashed_expr "J"
    let k = get_hashed_expr "K"
    let l = get_hashed_expr "L"
    let m = get_hashed_expr "M"

    assertEqual "hash H == hash K" (hash h) (hash k)
    assertEqual "hash I == hash J" (hash i) (hash j)
    assertNotEqual "hash H /= hash L" (hash h) (hash l)
    -- position matters within the same tycon
    assertNotEqual "hash L /= hash M" (hash l) (hash m)

    let t7  = get_hashed_expr "T7"
    let t8  = get_hashed_expr "T8"
    let t9  = get_hashed_expr "T9"
    let t10 = get_hashed_expr "T10"

    assertEqual "hash T7 == hash T8" (hash t7) (hash t8)
    assertNotEqual "hash T7 /= hash T9" (hash t7) (hash t9)
    assertNotEqual "hash T7 /= hash T10" (hash t7) (hash t10)
    assertNotEqual "hash T9 /= hash T10" (hash t9) (hash t10)

    let o = get_hashed_expr "O"
    let p = get_hashed_expr "P"
    let q = get_hashed_expr "Q"
    let r = get_hashed_expr "R"

    assertEqual "hash O == hash P" (hash o) (hash p)
    assertNotEqual "hash O /= hash Q" (hash o) (hash q)
    assertNotEqual "hash O /= hash R" (hash o) (hash r)

    let t11 = get_hashed_expr "T11"
    let t12 = get_hashed_expr "T12"

    assertEqual "hash T11 == hash T12" (hash t11) (hash t12)
    assertNotEqual "hash T11 /= hash T4" (hash t11) (hash t4)
    assertNotEqual "hash T11 /= hash T5" (hash t11) (hash t5)

    let t13 = get_hashed_expr "T13"
    let t14 = get_hashed_expr "T14"

    assertEqual "hash T13 == hash T14" (hash t13) (hash t14)
    assertNotEqual "hash T13 /= hash T6" (hash t13) (hash t6)

    let s = get_hashed_expr "S"
    let t = get_hashed_expr "T"
    let u = get_hashed_expr "U"
    let v = get_hashed_expr "V"

    assertEqual "hash S == hash V" (hash s) (hash v)
    assertEqual "hash T == hash U" (hash t) (hash u)
    assertNotEqual "hash S /= hash H" (hash s) (hash h)
    -- T and I look similar but produce different Types,
    -- therefore hash is different
    assertNotEqual "hash T /= hash I" (hash t) (hash i)

    let w = get_hashed_expr "W"
    let x = get_hashed_expr "X"
    let y = get_hashed_expr "Y"
    let z = get_hashed_expr "Z"

    assertEqual "hash W == hash Z" (hash w) (hash z)
    assertEqual "hash X == hash Y" (hash x) (hash y)
    assertNotEqual "hash W /= hash Y" (hash w) (hash y)
    assertNotEqual "hash X /= hash Z" (hash x) (hash z)

    let t15 = get_hashed_expr "T15"
    let t16 = get_hashed_expr "T16"
    let t17 = get_hashed_expr "T17"

    assertEqual "hash T15 == hash T16" (hash t15) (hash t16)
    assertNotEqual "hash T15 /= hash T17" (hash t15) (hash t17)

    let aa = get_hashed_expr "AA"
    let ab = get_hashed_expr "AB"
    let ac = get_hashed_expr "AC"

    assertEqual "hash AA == hash AB" (hash aa) (hash ab)
    assertNotEqual "hash AA /= hash AC" (hash aa) (hash ac)
    
    let t18 = get_hashed_expr "T18"
    let t19 = get_hashed_expr "T19"
    let t20 = get_hashed_expr "T20"

    assertEqual "hash T18 == hash T19" (hash t18) (hash t19)
    assertNotEqual "hash T18 == hash T20" (hash t18) (hash t20)
    pass

test4 :: IO ()
test4 = do
    ctx <- init_test 4 "recursive data types"
    let get_hashed_expr = flip (get_hashed_expr') ctx

    let mylist = get_hashed_expr "MyList"
    let t1 = get_hashed_expr "T1"
    let t2 = get_hashed_expr "T2"
    let t3 = get_hashed_expr "T3"

    assertEqual "hash MyList == hash T1" (hash mylist) (hash t1)
    assertNotEqual "hash T1 /= hash T2" (hash t1) (hash t2)
    assertNotEqual "hash T1 /= hash T3" (hash t1) (hash t3)
    assertNotEqual "hash T2 /= hash T3" (hash t2) (hash t3)

    let x = get_hashed_expr "X"
    let y = get_hashed_expr "Y"
    let a = get_hashed_expr "A"
    let b = get_hashed_expr "B"
    let c = get_hashed_expr "C"
    let d = get_hashed_expr "D"

    assertNotEqual "hash X /= hash A" (hash x) (hash a)
    assertNotEqual "hash X /= hash C" (hash x) (hash c)
    assertNotEqual "hash A /= hash C" (hash a) (hash c)
    assertNotEqual "hash Y /= hash B" (hash y) (hash b)
    assertNotEqual "hash Y /= hash D" (hash y) (hash d)
    assertNotEqual "hash B /= hash D" (hash b) (hash d)

    let t4 = get_hashed_expr "T4"
    let t5 = get_hashed_expr "T5"
    let t6 = get_hashed_expr "T6"

    assertEqual "hash T4 == hash T5" (hash t4) (hash t5)
    assertEqual "hash T5 == hash T6" (hash t5) (hash t6)
    assertEqual "hash T6 == hash T4" (hash t6) (hash t4)

    let e = get_hashed_expr "E"
    let f = get_hashed_expr "F"
    let g = get_hashed_expr "G"
    let h = get_hashed_expr "H"
    let i = get_hashed_expr "I"
    let j = get_hashed_expr "J"

    assertEqual "hash E == hash G" (hash e) (hash g)
    assertEqual "hash F == hash H" (hash f) (hash h)
    assertEqual "hash H == hash I" (hash h) (hash i)

    let t7 = get_hashed_expr "T7"
    let t9 = get_hashed_expr "T9"

    assertNotEqual "hash T7 /= hash T9" (hash t7) (hash t9)
    pass

test5 :: IO ()
test5 = do
    ctx <- init_test 5 "typeclasses"
    let get_hashed_expr = flip (get_hashed_expr') ctx

    let c1 = get_hashed_expr "C1"
    let c2 = get_hashed_expr "C2"
    let c3 = get_hashed_expr "C3"

    assertEqual "hash C1 == hash C2" (hash c1) (hash c2)
    assertNotEqual "hash C1 /= hash C3" (hash c1) (hash c3)

    pass

run_tests :: IO ()
run_tests = do
    test1
    test2
    test3
    test4
    test5

    putStrLn "all tests passed"


-- ETC

run_tests' :: IO ()
run_tests' = do
    compile_file' "tests/test2.hs"

scratch :: IO ()
scratch = do
    ctx <- compile_file "tests/scratch.hs" debug_output
    let get_hashed_expr = flip (get_hashed_expr') ctx
    
    let t = get_hashed_expr "T1"
    let tdep = fromJust $ get_tyDepGraph t
    putStrLn $ show tdep

scratch' :: IO ()
scratch' = compile_file' "tests/scratch.hs"
