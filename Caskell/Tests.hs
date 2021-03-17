
module Caskell.Tests
(
    run_tests,
    test1,
    test2,
    test3,
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

debug_output = True

-- utils
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
    let i = get_hashed_expr "I"

    assertNotEqual "hash A /= hash B" (hash a) (hash b)
    assertNotEqual "hash C /= hash D" (hash c) (hash d)
    assertEqual "hash A == hash C" (hash a) (hash c)
    assertEqual "hash B == hash D" (hash b) (hash d)

    pass

run_tests :: IO ()
run_tests = do
    test1
    test2
    test3

    putStrLn "all tests passed"


-- ETC

run_tests' :: IO ()
run_tests' = do
    compile_file' "tests/test2.hs"

scratch :: IO ()
scratch = do
    ctx <- compile_file "tests/scratch.hs" debug_output
    let get_hashed_expr = flip (get_hashed_expr') ctx
    
    let a = get_hashed_expr "A"
    let cores = Data.Map.MultiKey.toList $ hash_core_data a

    let uq = Unique.getKey . fromJust . uniq
    let uref = unique_definition_ref a
    let crefs = map core_definition_ref cores
    let refname x = case x of
                       Just hd -> "Just " ++ hashed_data_typename hd ++ " " ++ (show $ uq hd)
                       Nothing -> "Nothing"

    let showcore x = short_hash_data_name x ++ " " ++ hashed_data_typename x ++ " " ++ (show $ uq x)

    putStrLn $  refname $core_definition_ref $ fromJust uref
    putStrLn $ show $ map refname crefs
    putStrLn $ show $ map (showcore .hash_data) cores

    putStrLn $ show a

scratch' :: IO ()
scratch' = compile_file' "tests/scratch.hs"
