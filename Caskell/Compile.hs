
module Caskell.Compile
    (
        compile_file,
        run_tests,
        run_tests'
    ) where

import qualified FastString as FS
import qualified GHC
import qualified CoreSyn
import qualified DynFlags
import qualified Var
import qualified Name
import Outputable (Outputable, showPpr, ppr, showSDocUnsafe)
import qualified GHC.Paths as Paths

import Control.Monad.State
import Caskell.Hash
import Caskell.CoreHash
import Caskell.Context

prepare_dyn_flags :: GHC.Ghc a -> GHC.Ghc a
prepare_dyn_flags ga = do
    dflags <- DynFlags.getDynFlags
    let dflags2 = dflags {
        DynFlags.ghcLink   = DynFlags.NoLink
      , DynFlags.hscTarget = DynFlags.HscNothing
      , DynFlags.ghcMode   = DynFlags.OneShot
      }
    GHC.setSessionDynFlags dflags2
    ga

run_ghc_with_libpath :: GHC.Ghc a -> IO a
run_ghc_with_libpath ga = do
    let ga2 = prepare_dyn_flags ga
    GHC.runGhc (Just Paths.libdir) ga2

showPpr' :: GHC.CoreModule -> GHC.Ghc String
showPpr' a = (flip showPpr) a <$> DynFlags.getDynFlags

pretty_print_binds :: GHC.CoreModule -> GHC.Ghc String
pretty_print_binds (GHC.CoreModule _ _ binds _) = (flip showPpr) binds <$> DynFlags.getDynFlags

run_context :: CtxMonad () -> IO Context
run_context s = execStateT s $ empty_context

hash_module' :: GHC.CoreModule -> GHC.Ghc (IO Context)
hash_module' mod = return $ run_context $ hash_module mod


-- toStr :: (Functor m, Monad m) => CoreModule -> m String
--toStr a = return $ coreToStr a

-- compileToCoreModule :: GhcMonad m => FilePath -> m CoreModule
compile_file :: String -> IO (Context)
compile_file file = do
    let ghcCore = GHC.compileToCoreModule file
    -- let ghc = ghcCore >>= pretty_print_binds
    let ghc = ghcCore >>= hash_module'
    r <- run_ghc_with_libpath ghc
    r

test1 :: IO ()
test1 = do
    putStrLn "\n================="
    putStrLn "TEST 1"
    putStrLn "================="
    ctx <- compile_file "tests/test1.hs"

    putStrLn $ show ctx
    return ()

test2 :: IO ()
test2 = do
    putStrLn "\n================="
    putStrLn "TEST 2"
    putStrLn "================="
    ctx <- compile_file "tests/test2.hs"

    putStrLn $ show $ lookup_name "int5" ctx
    putStrLn $ show $ lookup_name "int3" ctx
    putStrLn $ show $ lookup_name "float3'14" ctx
    putStrLn $ show $ lookup_name "float2'71" ctx
    putStrLn $ show $ lookup_name "double3'14" ctx
    putStrLn $ show $ lookup_name "double2'71" ctx
    putStrLn $ show $ lookup_name "charA" ctx
    putStrLn $ show $ lookup_name "charB" ctx
    putStrLn $ show $ lookup_name "stringHello" ctx
    putStrLn $ show $ lookup_name "stringWorld" ctx
    return ()

run_tests :: IO ()
run_tests = do
    test1
    test2




-- ETC
compile_file' :: String -> IO ()
compile_file' file = do
    let ghcCore = GHC.compileToCoreModule file
    let ghc = ghcCore >>= pretty_print_binds
    let ret = run_ghc_with_libpath ghc
    putStrLn =<< ret
    return ()

run_tests' :: IO ()
run_tests' = do
    compile_file' "tests/test1.hs"
