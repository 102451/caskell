
module Caskell.Tests
(
    run_tests,
    test1, -- literals, references
    test2, -- literals
    test3, -- types
    test4, -- recursive types
    test5, -- typeclasses
    test6, -- functions
    test7, -- recusive functions
    test8, -- class instance, coercion, cast
    test9, -- newtypes

    class_instance_fail_test,
    run_tests',

    scratch,
    scratch'
) where

import Unique
import Data.List
import Data.Maybe
import Data.Map.MultiKey
import Control.Monad
import Control.Exception
import System.IO

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

init_test :: Int -> String -> IO (Context)
init_test num description = do
    compile_file ("tests/test" ++ show num ++ ".hs") debug_output

pass :: IO ()
pass = putStrLn "passed"

-- parser stuff
data Test = Test
    { test_header :: String
    , test_asserts :: [AssertExpr]
    }
    deriving (Eq, Show)

data AssertExpr = AssertExpr
    { ae_type :: AssertType
    , ae_val1 :: AssertVal
    , ae_val2 :: AssertVal
    , ae_description :: String
    }
    deriving (Eq, Show)

newtype AssertVal = AssertVal String deriving (Eq, Show) -- just "hash X"
assertVal (AssertVal n) = n
data AssertType = AssertEqual | AssertNotEqual deriving (Eq, Show)

parse_test :: String -> IO Test
parse_test filepath = do
    contents <- readFile filepath
    let ls = lines contents
    let filtered = filter (isPrefixOf "--" . dropWhile (==' ')) ls

    asserts <- read_asserts_from_test filtered
    let test = Test "" asserts

    if (not $ Data.List.null filtered) then do
        let headr = dropWhile (==' ') $ dropWhile (=='-') $ dropWhile (==' ') $ head filtered
        return $ Test headr asserts
    else
        return test
    

read_asserts_from_test :: [String] -> IO [AssertExpr]
read_asserts_from_test lines = do
    let mexprs = map (parse_assert_from_line) lines
    let mexprs' = filter (isJust) mexprs
    let exprs = map (fromJust) mexprs'

    return exprs
    
parse_assert_from_line :: String -> Maybe AssertExpr
parse_assert_from_line [] = Nothing
parse_assert_from_line ('-':'-':t) = parse_assert_from_line t
parse_assert_from_line (' ':t) = parse_assert_from_line t
parse_assert_from_line ('a':'s':'s':'e':'r':'t':t) = do
    (expr, _) <- parse_assert t
    return expr
parse_assert_from_line _ = Nothing

parse_assert :: String -> Maybe (AssertExpr, String)
parse_assert [] = Nothing
parse_assert (' ':t) = parse_assert t
parse_assert t = do
    (e1, t2) <- parse_assert_val t
    (op, t3) <- parse_assert_type t2
    (e2, t4) <- parse_assert_val t3
    let desc = dropWhile (==' ') t4
    return (AssertExpr op e1 e2 desc, "")

parse_assert_val :: String -> Maybe (AssertVal, String)
parse_assert_val [] = Nothing
parse_assert_val (' ':t) = parse_assert_val t
parse_assert_val ('h':'a':'s':'h':t) = do
    let t' = dropWhile (==' ') t
    let name = takeWhile (/=' ') t'

    if Data.List.null name then
        Nothing
    else
        return (AssertVal name, drop (length name) t')
parse_assert_val _ = Nothing

parse_assert_type :: String -> Maybe (AssertType, String)
parse_assert_type [] = Nothing
parse_assert_type (' ':t) = parse_assert_type t
parse_assert_type ('=':'=':t) = Just (AssertEqual, t)
parse_assert_type ('/':'=':t) = Just (AssertNotEqual, t)
parse_assert_type _ = Nothing

-- parsed assert stuff
assert_test_of_context :: Context -> AssertExpr -> IO ()
assert_test_of_context ctx (AssertExpr t v1 v2 desc) = do
    let e1 = assertVal v1
    let e2 = assertVal v2
    let he1 = get_hashed_expr' e1 ctx 
    let he2 = get_hashed_expr' e2 ctx
    let h1 = hash he1
    let h2 = hash he2
    let s = assertion_text t v1 v2

    when (not $ Data.List.null desc) $ do
        putStrLn $ "checking " ++ desc
        
    assertT t s h1 h2

assertion_text :: AssertType -> AssertVal -> AssertVal -> String
assertion_text t v1 v2 = s where
    ts = case t of
            AssertEqual -> "=="
            AssertNotEqual -> "/="
    s1 = case v1 of
            AssertVal n -> "hash " ++ n
    s2 = case v2 of
            AssertVal n -> "hash " ++ n
    s = s1 ++ " " ++ ts ++ " " ++ s2

assertT :: Eq a => AssertType -> String -> a -> a -> IO ()
assertT AssertEqual = assertEqual
assertT AssertNotEqual = assertNotEqual
    
assertEqual :: Eq a => String -> a -> a -> IO ()
assertEqual assertion x y = unless (x == y) (error $ "assertion failed: " ++ assertion)

assertNotEqual :: Eq a => String -> a -> a -> IO ()
assertNotEqual assertion x y = when (x == y) (error $ "assertion failed: " ++ assertion)
    
run_asserts :: Context -> [AssertExpr] -> IO (Int)
run_asserts ctx asserts =
    if Data.List.null asserts then do
      putStrLn "warning: no asserts found"
      return 0
    else do
      mapM_ (assert_test_of_context ctx) asserts
      return (length asserts)


-- error when not found
get_hashed_expr' :: String -> Context -> UniqueHash
get_hashed_expr' var ctx = fromJust $ lookup_name var ctx

-- generic test
test :: String -> IO (Int)
test testfile = do
    ctx <- compile_file testfile debug_output
    test <- parse_test testfile

    putStrLn $ "\n" ++ test_header test
    i <- run_asserts ctx (test_asserts test)
    putStrLn $ "\npassed " ++ show i ++ " tests"
    return i

test1 = test "tests/test1.hs"
test2 = test "tests/test2.hs"
test3 = test "tests/test3.hs"
test4 = test "tests/test4.hs"
test5 = test "tests/test5.hs"
test6 = test "tests/test6.hs"
test7 = test "tests/test7.hs"
test8 = test "tests/test8.hs"
test9 = test "tests/test9.hs"
    
class_instance_fail_test :: IO (Int)
class_instance_fail_test = test "tests/class_instance_fail.hs"

run_tests :: IO ()
run_tests = do
    testscount <- sequence [test1, test2, test3, test4, test5, test6, test7, test8, class_instance_fail_test, test9]
    let numtests = sum testscount

    putStrLn "-----------------------"
    putStrLn $ "passed all " ++ show numtests ++ " tests"


-- ETC

run_tests' :: IO ()
run_tests' = do
    compile_file' "tests/test2.hs"

scratch :: IO ()
scratch = do
    ctx <- compile_file "tests/scratch.hs" debug_output
    let get_hashed_expr = flip (get_hashed_expr') ctx
    
    putStrLn $ show ctx

scratch' :: IO ()
scratch' = compile_file' "tests/scratch.hs"
