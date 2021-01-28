{-|
    this module uses the Hash module to hash a GHC Core Module

    https://downloads.haskell.org/~ghc/8.8.1/docs/html/libraries/ghc-8.8.1/CoreSyn.html#t:Expr
|-}

module Caskell.CoreHash
(
    uniqueBytes
) where

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified GHC as GHC
import qualified CoreSyn as Core
import qualified Literal as Literal
import TyCoRep
import qualified Var as Var
import DynFlags
import BasicTypes
import Outputable (Outputable, showPpr)

import Data.Functor
import Data.Coerce
import Data.Typeable
import Data.Word
import FastString
import Unique

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

instance BinarySerializable Literal.LitNumType where
    toBytes Literal.LitNumInteger = toBytes (0x00 :: Word8)
    toBytes Literal.LitNumNatural = toBytes (0x01 :: Word8)
    toBytes Literal.LitNumInt     = toBytes (0x02 :: Word8)
    toBytes Literal.LitNumInt64   = toBytes (0x03 :: Word8)
    toBytes Literal.LitNumWord    = toBytes (0x04 :: Word8)
    toBytes Literal.LitNumWord64  = toBytes (0x05 :: Word8)

instance BinarySerializable BasicTypes.FunctionOrData where
    toBytes BasicTypes.IsFunction = toBytes (0x00 :: Word8)
    toBytes BasicTypes.IsData     = toBytes (0x01 :: Word8)

instance Hashable Literal.Literal where
    typeID (Literal.LitChar _)          = 0x00002000
    typeID (Literal.LitNumber _ _ _)    = 0x00002001
    typeID (Literal.LitString _)        = 0x00002002
    typeID (Literal.LitNullAddr)        = 0x00002003
    typeID (Literal.LitRubbish)         = 0x00002004
    typeID (Literal.LitFloat _)         = 0x00002005
    typeID (Literal.LitDouble _)        = 0x00002006
    typeID (Literal.LitLabel _ _ _)     = 0x00002007

    uniqueBytes x = bytes where
        tb = typeID' x
        bts = case x of
          Literal.LitChar char -> uniqueBytes char
          Literal.LitNumber l i t -> toBytes l ++ toBytes i ++ uniqueBytes t
          Literal.LitString a -> [a]
          Literal.LitNullAddr -> []
          Literal.LitRubbish -> []
          Literal.LitFloat ratio -> uniqueBytes ratio
          Literal.LitDouble ratio -> uniqueBytes ratio
          Literal.LitLabel s m fod -> uniqueBytes s ++ uniqueBytes m ++ toBytes fod
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts

instance Hashable Type where
    typeID (TyVarTy _)    = 0x00003000
    typeID (AppTy _ _)    = 0x00003001
    typeID (TyConApp _ _) = 0x00003002
    typeID (ForAllTy _ _) = 0x00003003
    typeID (FunTy _ _)    = 0x00003004
    typeID (LitTy _)      = 0x00003005
    typeID (CastTy _ _)   = 0x00003006
    typeID (CoercionTy _) = 0x00003007

    uniqueBytes x = bytes where
        tb = typeID' x
        bts = case x of
          TyVarTy var -> uniqueBytes var
          AppTy t1 t2 -> uniqueBytes t1 ++ uniqueBytes t2
          TyConApp tcon l -> uniqueBytes tcon ++ concatMap (uniqueBytes) l
          ForAllTy bndr t -> uniqueBytes bndr ++ uniqueBytes t
          FunTy t1 t2 -> uniqueBytes t1 ++ uniqueBytes t2
          LitTy tlit -> uniqueBytes tlit
          CastTy t kcoer -> uniqueBytes t ++ uniqueBytes kcoer
          CoercionTy coer -> uniqueBytes coer
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts

instance Hashable Var.Var where
    typeID = const 0x00004000
    uniqueBytes x = bytes where
        tb = typeID' x
        bts = uniqueBytes $ Unique.getKey $ Var.varUnique x
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts

instance Hashable ArgFlag where
    typeID Inferred  = 0x00004001
    typeID Specified = 0x00004002
    typeID Required  = 0x00004003
    uniqueBytes = typeID'

instance (Hashable a, Hashable b) => Hashable (Var.VarBndr a b) where
    typeID = const 0x00004004
    uniqueBytes (Var.Bndr x y) = bytes where
        tb = typeID' (Var.Bndr x y)
        bts = uniqueBytes (x, y)
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts

instance Hashable TyLit where
    typeID (NumTyLit _) = 0x00005000
    typeID (StrTyLit _) = 0x00005001
    uniqueBytes x = bytes where
        tb = typeID' x
        bts = case x of
          NumTyLit i -> uniqueBytes i
          StrTyLit s -> uniqueBytes s
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts

instance Hashable Coercion where
    typeID x = case x of
      TyCoRep.Refl _ -> 0x00006000
      -- TODO: complete

    uniqueBytes x = bytes where
        tb = typeID' x
        bts = case x of
          TyCoRep.Refl t -> uniqueBytes t
            -- TODO: complete
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts

-- TODO: TyCon
