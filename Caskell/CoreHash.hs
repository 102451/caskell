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
import qualified Name
import qualified CoreSyn as Core
import qualified Literal as Literal
import qualified Var as Var
import qualified IdInfo as IdInfo
import qualified PatSyn as PatSyn
import BasicTypes
import Outputable (Outputable, showPpr)

import DynFlags
import TyCon
import TyCoRep
import CoAxiom

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
    typeID x
        | Var.isTyVar x   = 0x00004000
        | Var.isTcTyVar x = 0x00004001
        | Var.isId x      = 0x00004002
        | True            = 0x00004003

    uniqueBytes x = bytes where
        tb = typeID' x
        bts'
          | Var.isTyVar x   = []
          | Var.isTcTyVar x = uniqueBytes (Var.tcTyVarDetails x)
          | Var.isId x = uniqueBytes (Var.idDetails x)
                      ++ uniqueBytes (Var.idInfo x)
        bts = uniqueBytes (Var.varType x) ++ bts' -- name and unique are useless
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts

instance Hashable ArgFlag where
    typeID Inferred  = 0x00004100
    typeID Specified = 0x00004101
    typeID Required  = 0x00004102
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
      TyCoRep.Refl _            -> 0x00006000
      TyCoRep.GRefl _ _ _       -> 0x00006001
      TyCoRep.TyConAppCo _ _ _  -> 0x00006002
      TyCoRep.AppCo _ _         -> 0x00006003
      TyCoRep.ForAllCo _ _ _    -> 0x00006004
      TyCoRep.FunCo _ _ _       -> 0x00006005
      TyCoRep.CoVarCo _         -> 0x00006006
      TyCoRep.CoVarCo _         -> 0x00006007
      TyCoRep.AxiomInstCo _ _ _ -> 0x00006008
      TyCoRep.AxiomRuleCo _ _   -> 0x00006009
      TyCoRep.UnivCo _ _ _ _    -> 0x0000600A
      TyCoRep.SymCo _           -> 0x0000600B
      TyCoRep.TransCo _ _       -> 0x0000600C
      TyCoRep.NthCo _ _ _       -> 0x0000600D
      TyCoRep.LRCo _ _          -> 0x0000600E
      TyCoRep.InstCo _ _        -> 0x0000600F
      TyCoRep.KindCo _          -> 0x00006010
      TyCoRep.SubCo _           -> 0x00006011
      TyCoRep.HoleCo _          -> 0x00006012

    uniqueBytes x = bytes where
        tb = typeID' x
        bts = case x of
          TyCoRep.Refl t -> uniqueBytes t
          TyCoRep.GRefl role t c -> toBytes role ++ uniqueBytes t ++ uniqueBytes c
          TyCoRep.TyConAppCo role tcon l -> toBytes role ++ uniqueBytes tcon ++ uniqueBytes l
          TyCoRep.AppCo c1 c2 -> uniqueBytes c1 ++ uniqueBytes c2
          TyCoRep.ForAllCo vtycovar kc c -> uniqueBytes vtycovar ++ uniqueBytes kc ++ uniqueBytes c
          TyCoRep.FunCo role c1 c2 -> toBytes role ++ uniqueBytes c1 ++ uniqueBytes c2
          TyCoRep.CoVarCo cv -> uniqueBytes cv
          TyCoRep.AxiomInstCo coax index l -> uniqueBytes coax ++ uniqueBytes index ++ uniqueBytes l
          TyCoRep.AxiomRuleCo coaxrule l -> uniqueBytes coaxrule ++ uniqueBytes l
          TyCoRep.UnivCo univ role t1 t2 -> uniqueBytes univ ++ toBytes role ++ uniqueBytes t1 ++ uniqueBytes t2
          TyCoRep.SymCo c -> uniqueBytes c
          TyCoRep.TransCo c1 c2 -> uniqueBytes c1 ++ uniqueBytes c2
          TyCoRep.NthCo role i c -> toBytes role ++ uniqueBytes i ++ uniqueBytes c
          TyCoRep.LRCo lr c -> toBytes lr ++ uniqueBytes c
          TyCoRep.InstCo c1 c2 -> uniqueBytes c1 ++ uniqueBytes c2
          TyCoRep.KindCo c -> uniqueBytes c
          TyCoRep.SubCo c -> uniqueBytes c
          TyCoRep.HoleCo _ -> [] -- ignore

        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts

instance BinarySerializable BasicTypes.LeftOrRight where
    toBytes x = toBytes y where
        y = case x of
          BasicTypes.CLeft  -> 0x00 :: Word8
          BasicTypes.CRight -> 0x01 :: Word8

instance BinarySerializable CoAxiom.Role where
    toBytes x = toBytes y where
        y = case x of
          CoAxiom.Nominal          -> 0x00 :: Word8
          CoAxiom.Representational -> 0x01 :: Word8
          CoAxiom.Phantom          -> 0x02 :: Word8

instance Hashable MCoercion where
    typeID x = case x of
      TyCoRep.MRefl -> 0x00007000
      TyCoRep.MCo _ -> 0x00007001

    uniqueBytes x = bytes where
        tb = typeID' x
        bts = case x of
          TyCoRep.MRefl -> []
          TyCoRep.MCo c -> uniqueBytes c

        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts

instance Hashable (CoAxiom.CoAxiom br) where
    typeID = const 0x00008000
    uniqueBytes x = bytes where
        tb = typeID' x
        bts = toBytes     (co_ax_role x)
           ++ uniqueBytes (co_ax_tc x)
           ++ uniqueBytes (co_ax_branches x)
           ++ uniqueBytes (co_ax_implicit x)
           -- Unique is not unique across builds
           -- ++ uniqueBytes (co_ax_unique x)
           -- Name is not part of content
           -- ++ uniqueBytes (co_ax_name x)

        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts

{-|
instance Hashable Name.Name where
    typeID = const 0x00009000
    uniqueBytes x = bytes where
        tb = typeID' x
        bts = uniqueBytes (Name.nameUnique x)
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts
|-}

instance Hashable (Branches br) where
    typeID = const 0x0000A000
    uniqueBytes x = bytes where
        tb = typeID' x
        bts = uniqueBytes $ unMkBranches x
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts

instance Hashable CoAxBranch where
    typeID = const 0x0000B000
    uniqueBytes x = bytes where
        tb = typeID' x
        -- SrcLoc is ignored
        bts = uniqueBytes (cab_tvs x)
           ++ uniqueBytes (cab_eta_tvs x)
           ++ uniqueBytes (cab_cvs x)
           ++ concatMap (toBytes) (cab_roles x)
           ++ uniqueBytes (cab_lhs x)
           ++ uniqueBytes (cab_rhs x)
           ++ uniqueBytes (cab_incomps x)
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts

instance Hashable CoAxiomRule where
    typeID = const 0x0000C000
    uniqueBytes x = bytes where
        tb = typeID' x
        bts = uniqueBytes (coaxrName x)
           ++ concatMap (toBytes) (coaxrAsmpRoles x)
           ++ toBytes (coaxrRole x)
           -- ++ uniqueBytes (coaxrProves x)
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts

instance Hashable TyCoRep.UnivCoProvenance where
    typeID x = case x of
      TyCoRep.UnsafeCoerceProv -> 0x0000D000
      TyCoRep.PhantomProv _    -> 0x0000D001
      TyCoRep.ProofIrrelProv _ -> 0x0000D002
      TyCoRep.PluginProv _     -> 0x0000D003

    uniqueBytes x = bytes where
        tb = typeID' x
        bts = case x of
          TyCoRep.UnsafeCoerceProv  -> []
          TyCoRep.PhantomProv kc    -> uniqueBytes kc
          TyCoRep.ProofIrrelProv kc -> uniqueBytes kc
          TyCoRep.PluginProv s      -> uniqueBytes s
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts
    
instance Hashable IdInfo.IdDetails where
    typeID x = case x of
      IdInfo.VanillaId       -> 0x0000E000
      IdInfo.RecSelId{}      -> 0x0000E001
      IdInfo.DataConWorkId _ -> 0x0000E002
      IdInfo.DataConWrapId _ -> 0x0000E003
      IdInfo.ClassOpId _     -> 0x0000E004
      IdInfo.PrimOpId _      -> 0x0000E005
      IdInfo.FCallId _       -> 0x0000E006
      IdInfo.TickBoxOpId _   -> 0x0000E007
      IdInfo.DFunId _        -> 0x0000E008
      IdInfo.CoVarId         -> 0x0000E009
      IdInfo.JoinId _        -> 0x0000E00A

    uniqueBytes x = bytes where
        tb = typeID' x
        bts = case x of
          IdInfo.VanillaId        -> []
          IdInfo.RecSelId{}       -> uniqueBytes (IdInfo.sel_tycon x) ++ toBytes (IdInfo.sel_naughty x)
          IdInfo.DataConWorkId dc -> uniqueBytes dc
          IdInfo.DataConWrapId dc -> uniqueBytes dc
          IdInfo.ClassOpId cls    -> uniqueBytes cls
          IdInfo.PrimOpId prim    -> uniqueBytes prim
          IdInfo.FCallId _        -> [] -- ignored
          IdInfo.TickBoxOpId _    -> [] -- ignored
          IdInfo.DFunId b         -> toBytes b
          IdInfo.CoVarId          -> []
          IdInfo.JoinId ar        -> toBytes ar
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts

instance Hashable IdInfo.RecSelParent where
    typeID x = case x of
      IdInfo.RecSelData _   -> 0x0000F000
      IdInfo.RecSelPatSyn _ -> 0x0000F001

    uniqueBytes x = bytes where
        tb = typeID' x
        bts = case x of
          IdInfo.RecSelData tc    -> uniqueBytes tc
          IdInfo.RecSelPatSyn pat -> uniqueBytes pat

        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts

instance Hashable PatSyn.PatSyn where
    typeID = const 0x00010000
    uniqueBytes x = bytes where
        tb = typeID' x
        (tvar, req, extvar, prov, arg_tys, res_ty) = PatSyn.patSynSig x
        bts = uniqueBytes tvar
           ++ uniqueBytes req
           ++ uniqueBytes extvar
           ++ uniqueBytes prov
           ++ uniqueBytes arg_tys
           ++ uniqueBytes res_ty

        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts


-- TODO: TcType.TcTyVarDetails
-- TODO: IdInfo.IdInfo
-- TODO: GHC.DataCon
-- TODO: GHC.Class
-- TODO: PrimOp.PrimOp

-- TODO: TyCon
{-|
instance Hashable TyCon.TyCon where
    typeID x = case x of
      FunTyCon{} -> 0x0000E000

    uniqueBytes x = []
|-}
