
module Caskell.Compile
    (
        compileFile
    ) where

import FastString
import GHC
import DynFlags
import Outputable (Outputable, showPpr)
import qualified GHC.Paths as Paths

import Data.Functor
import Data.Coerce
import Data.Typeable

runGhc' :: Ghc a -> IO a
runGhc' ga = do
    runGhc (Just Paths.libdir) $ do
        dflags <- getDynFlags
        let dflags2 = dflags { ghcLink   = NoLink
                             , hscTarget = HscNothing
                             , ghcMode   = OneShot
                             }
        setSessionDynFlags dflags2
        ga

showPpr' :: (Functor m, HasDynFlags m) => CoreModule -> m String
showPpr' a = (flip showPpr) a <$> getDynFlags

moduleToStr :: Module -> String
moduleToStr mod =
    moduleNameString $ moduleName mod

coreToStr :: CoreModule -> String
coreToStr (CoreModule mod types binds _) =
    moduleToStr mod

toStr :: (Functor m, Monad m) => CoreModule -> m String
toStr a = return $ coreToStr a

-- compileToCoreModule :: GhcMonad m => FilePath -> m CoreModule
compileFile :: String -> IO ()
compileFile file = do
    let ghcCore = compileToCoreModule file
    let a = runGhc' (ghcCore >>= toStr)
    putStrLn =<< a
    --let str = coreToStr =<< ghcCore
    --putStrLn =<< str
