
module Caskell.Compile
    (
        compile_file,
        run_tests
    ) where

import qualified FastString as FS
import qualified GHC
import qualified CoreSyn
import qualified DynFlags
import qualified Var
import qualified Name
import Outputable (Outputable, showPpr, ppr, showSDocUnsafe)
import qualified GHC.Paths as Paths

import Data.Functor
import Data.Coerce
import Data.Typeable

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

hash_expr :: (CoreSyn.Expr a) -> Hash
hash_expr x = getHash bytes where
    bytes = case x of
      CoreSyn.Lit lit -> uniqueBytes lit
      _ -> []

hash_bind :: CoreSyn.CoreBind -> (String, Hash)
hash_bind (CoreSyn.NonRec b expr) = (Name.nameStableString $ Var.varName b, hash_expr expr)
hash_bind (CoreSyn.Rec l) = undefined


hash_binds :: CoreSyn.CoreProgram -> StateCtx ()
hash_binds binds = do
    binds' <- get_bind_hashes
    set_bind_hashes $ map (hash_bind) binds
    

run_context :: StateCtx () -> Context
run_context s = execState s $ empty_context

hash_module :: GHC.CoreModule -> GHC.Ghc Context
hash_module (GHC.CoreModule _ _ binds _) = return $ run_context $ hash_binds binds


-- toStr :: (Functor m, Monad m) => CoreModule -> m String
--toStr a = return $ coreToStr a

-- compileToCoreModule :: GhcMonad m => FilePath -> m CoreModule
compile_file :: String -> IO ()
compile_file file = do
    let ghcCore = GHC.compileToCoreModule file
    -- let ghc = ghcCore >>= pretty_print_binds
    let ghc = ghcCore >>= hash_module
    let ret = run_ghc_with_libpath ghc
    putStrLn =<< fmap (show) ret


run_tests :: IO ()
run_tests = do
    compile_file "tests/test1.hs"
