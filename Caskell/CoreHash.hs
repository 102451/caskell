{-|
    this module uses the Hash module to hash a GHC Core Module

    https://downloads.haskell.org/~ghc/8.8.1/docs/html/libraries/ghc-8.8.1/CoreSyn.html#t:Expr
|-}

module Caskell.CoreHash
(
    uniqueBytes
) where

import FastString
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified GHC as GHC
import qualified CoreSyn as Core
import qualified Literal as Literal
import DynFlags
import Outputable (Outputable, showPpr)

import Data.Functor
import Data.Coerce
import Data.Typeable

import Caskell.Bytes
import Caskell.Hash


typeID' :: Hashable a => a -> [BS.ByteString]
typeID' = toBytes . typeID 

instance Hashable (Core.Expr b) where
    typeID (Core.Var _)          = 0x00001000
    typeID (Core.Lit _)          = 0x00001001
    typeID (Core.App _ _)        = 0x00001002
    typeID (Core.Lam _ _)        = 0x00001003
    typeID (Core.Let _ _)        = 0x00001004
    typeID (Core.Case _ _ _ _)   = 0x00001005
    typeID (Core.Cast _ _)       = 0x00001006
    typeID (Core.Tick _ _)       = 0x00001007
    typeID (Core.Type _)         = 0x00001008

    uniqueBytes x = bytes where
        tb = typeID' x
        bts = case x of
          Core.Lit lit -> uniqueBytes lit
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts

instance Hashable Literal.Literal where
    typeID (Literal.LitChar _)          = 0x00002000
    typeID (Literal.LitNumber _ _ _)    = 0x00002001
    typeID (Literal.LitString _)        = 0x00002002
    typeID (Literal.LitNullAddr)        = 0x00002003
    typeID (Literal.LitRubbish)         = 0x00002004
    typeID (Literal.LitFloat _)         = 0x00002005
    typeID (Literal.LitDouble _)        = 0x00002006
    typeID (Literal.LitLabel _ _ _)     = 0x00002007

    -- TODO
    uniqueBytes x = bytes where
        tb = typeID' x
        bts = case x of
          Literal.LitChar char -> uniqueBytes char
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts
