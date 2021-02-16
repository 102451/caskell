{-|
    this module uses the Hash module to hash a GHC Core Module

    https://downloads.haskell.org/~ghc/8.8.3/docs/html/libraries/ghc-8.8.3/CoreSyn.html#t:Expr
|-}

module Caskell.CoreHash
(
    uniqueBytes
) where

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified GHC
import qualified Name
import qualified CoreSyn as Core
import qualified Literal
import qualified Var
import qualified IdInfo
import qualified PatSyn
import qualified TcType
import qualified PrimOp
import qualified DataCon
import qualified Class
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
import Caskell.PrimOpHash


typeID' :: Hashable a => a -> [BS.ByteString]
typeID' = toBytes . typeID 

instance Hashable b => Hashable (Core.Expr b) where
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
          Core.Var var -> uniqueBytes var
          Core.Lit lit -> uniqueBytes lit
          Core.App e1 e2 -> uniqueBytes e1 ++ uniqueBytes e2
          Core.Lam b e -> uniqueBytes b ++ uniqueBytes e
          Core.Let b e -> uniqueBytes b ++ uniqueBytes e
          Core.Case e b t alts -> uniqueBytes e ++ uniqueBytes b ++ uniqueBytes t ++ uniqueBytes alts
          Core.Cast e coer -> uniqueBytes e ++ uniqueBytes coer
          Core.Tick tick e -> -- uniqueBytes tick ++ -- tickish is not useful
              uniqueBytes e
          Core.Type t -> uniqueBytes t
          Core.Coercion coer -> uniqueBytes coer
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

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
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

instance Hashable Type where
    typeID (TyVarTy _)    = 0x00003000
    typeID (AppTy _ _)    = 0x00003001
    typeID (TyConApp _ _) = 0x00003002
    typeID (ForAllTy _ _) = 0x00003003
    typeID (FunTy{})      = 0x00003004
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
          FunTy ft_af t1 t2 -> uniqueBytes ft_af ++ uniqueBytes t1 ++ uniqueBytes t2
          LitTy tlit -> uniqueBytes tlit
          CastTy t kcoer -> uniqueBytes t ++ uniqueBytes kcoer
          CoercionTy coer -> uniqueBytes coer
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

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
                      -- https://downloads.haskell.org/~ghc/8.8.4/docs/html/libraries/ghc-8.8.4/src/IdInfo.html#IdInfo
                      -- ++ uniqueBytes (Var.idInfo x) -- IdInfo may not be always present and is purely optional
        bts = uniqueBytes (Var.varType x) ++ bts' -- name and unique are useless
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

instance (Hashable a, Hashable b) => Hashable (Var.VarBndr a b) where
    typeID = const 0x00004004
    uniqueBytes (Var.Bndr x y) = bytes where
        tb = typeID' (Var.Bndr x y)
        bts = uniqueBytes (x, y)
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

instance Hashable ArgFlag where
    typeID Inferred  = 0x00004100
    typeID Specified = 0x00004101
    typeID Required  = 0x00004102
    uniqueBytes = typeID'

instance Hashable Var.AnonArgFlag where
    typeID Var.VisArg  = 0x00004200
    typeID Var.InvisArg = 0x00004201
    uniqueBytes = typeID'

instance Hashable TyLit where
    typeID (NumTyLit _) = 0x00005000
    typeID (StrTyLit _) = 0x00005001
    uniqueBytes x = bytes where
        tb = typeID' x
        bts = case x of
          NumTyLit i -> uniqueBytes i
          StrTyLit s -> uniqueBytes s
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

instance Hashable Coercion where
    typeID x = case x of
      TyCoRep.Refl _            -> 0x00006000
      TyCoRep.GRefl _ _ _       -> 0x00006001
      TyCoRep.TyConAppCo _ _ _  -> 0x00006002
      TyCoRep.AppCo _ _         -> 0x00006003
      TyCoRep.ForAllCo _ _ _    -> 0x00006004
      TyCoRep.FunCo _ _ _       -> 0x00006005
      TyCoRep.CoVarCo _         -> 0x00006006
      TyCoRep.AxiomInstCo _ _ _ -> 0x00006007
      TyCoRep.AxiomRuleCo _ _   -> 0x00006008
      TyCoRep.UnivCo _ _ _ _    -> 0x00006009
      TyCoRep.SymCo _           -> 0x0000600A
      TyCoRep.TransCo _ _       -> 0x0000600B
      TyCoRep.NthCo _ _ _       -> 0x0000600C
      TyCoRep.LRCo _ _          -> 0x0000600D
      TyCoRep.InstCo _ _        -> 0x0000600E
      TyCoRep.KindCo _          -> 0x0000600F
      TyCoRep.SubCo _           -> 0x00006010
      TyCoRep.HoleCo _          -> 0x00006011

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
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

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
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

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
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

-- IGNORE NAMES
instance Hashable Name.Name where
    typeID = const 0x00009000
    uniqueBytes _ = []
{-|
    uniqueBytes x = bytes where
        tb = typeID' x
        bts = uniqueBytes (Name.nameUnique x)
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts
|-}

instance Hashable (Branches br) where
    typeID = const 0x0000A000
    uniqueBytes x = bytes where
        tb = typeID' x
        bts = uniqueBytes $ unMkBranches x
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

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
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

instance Hashable CoAxiomRule where
    typeID = const 0x0000C000
    uniqueBytes x = bytes where
        tb = typeID' x
        bts = uniqueBytes (coaxrName x)
           ++ concatMap (toBytes) (coaxrAsmpRoles x)
           ++ toBytes (coaxrRole x)
           -- ++ uniqueBytes (coaxrProves x)
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

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
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts
    
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
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

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
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

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
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts


instance Hashable TcType.TcTyVarDetails where
    typeID x = case x of
      TcType.SkolemTv _ _   -> 0x00011000
      TcType.RuntimeUnk     -> 0x00011001
      TcType.MetaTv{}       -> 0x00011002

    uniqueBytes x = bytes where
        tb = typeID' x
        bts = case x of
          TcType.SkolemTv lv b -> toBytes lv ++ toBytes b
          TcType.RuntimeUnk -> []
          TcType.MetaTv{ TcType.mtv_info = info, TcType.mtv_tclvl = tclvl } ->
              toBytes info ++ toBytes tclvl

        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

instance BinarySerializable TcType.TcLevel where
    toBytes (TcType.TcLevel x) = toBytes x

instance BinarySerializable TcType.MetaInfo where
    toBytes x = toBytes b where
        b = case x of
          TcType.TauTv      -> 0x00 :: Word8
          TcType.TyVarTv    -> 0x01 :: Word8
          TcType.FlatMetaTv -> 0x02 :: Word8
          TcType.FlatSkolTv -> 0x03 :: Word8

-- DataCon.DataCon
-- Data Constructor
-- https://hackage.haskell.org/package/ghc-8.10.2/docs/DataCon.html#g:1
instance Hashable DataCon.DataCon where
    typeID = const 0x00012000
    uniqueBytes x = bytes where
        tb = typeID' x
        (tycovs, thet, argument_types, result_type) = DataCon.dataConSig x
        bts = uniqueBytes tycovs -- TODO: check if necessary
           ++ uniqueBytes thet -- TODO: check if necessary
           ++ uniqueBytes argument_types
           ++ uniqueBytes result_type

        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

-- Class.Class
-- TypeClass
instance Hashable Class.Class where
    typeID = const 0x00013000
    uniqueBytes x = bytes where
        tb = typeID' x
        (_, sc_theta, sc_sels, op_stuff) = Class.classBigSig x
        bts = uniqueBytes (Class.classTyCon x)
           ++ uniqueBytes sc_theta
           ++ uniqueBytes sc_sels
           ++ uniqueBytes op_stuff -- TODO: test this stuff

        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

instance Hashable ty => Hashable (DefMethSpec ty) where
    typeID x = case x of
      VanillaDM   -> 0x00014000
      GenericDM _ -> 0x00014001

    uniqueBytes x = bytes where
        tb = typeID' x
        bts = case x of
          VanillaDM    -> []
          GenericDM ty -> uniqueBytes ty

        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

instance Hashable a => Hashable (Core.Bind a) where
    typeID (Core.NonRec _ _) = 0x00015000
    typeID (Core.Rec _)      = 0x00015001

    uniqueBytes x = bytes where
        tb = typeID' x
        bts = case x of
          Core.NonRec b e -> uniqueBytes b ++ uniqueBytes e
          Core.Rec l -> uniqueBytes l

        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

instance Hashable Core.AltCon where
    typeID x = case x of
      Core.DataAlt _ -> 0x00016000
      Core.LitAlt _  -> 0x00016001
      Core.DEFAULT   -> 0x00016002

    uniqueBytes x = bytes where
        tb = typeID' x
        bts = case x of
          Core.DataAlt dc -> uniqueBytes dc
          Core.LitAlt l  -> uniqueBytes l
          Core.DEFAULT   -> []

        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

bytes_synTyConRhs x = b where
  y = TyCon.synTyConRhs_maybe x
  b = case y of
    Just rhs -> uniqueBytes rhs
    Nothing  -> []

-- TyCon
instance Hashable TyCon.TyCon where
    typeID x =
        if TyCon.isFunTyCon x then              0x00020000
        else if TyCon.isAlgTyCon x then         0x00020001
        else if TyCon.isTypeSynonymTyCon x then 0x00020002
        else if TyCon.isFamilyTyCon x then      0x00020003
        else if TyCon.isPrimTyCon x then        0x00020004
        else if TyCon.isPromotedDataCon x then  0x00020005
        -- only used during typechecking
        -- else if TyCon.isTcTyCon x then          0x00020006
        else 0x00020006

    uniqueBytes x = bytes where
        tb = typeID' x

        bndrs = uniqueBytes $ TyCon.tyConBinders x
        result_kind = uniqueBytes $ TyCon.tyConResKind x
        -- TyCon.tyConTyVars is a cached field of tyConBinders
        -- TyCon.tyConArity is a cached field of length (tyConBinders)

        bts =
          if TyCon.isAlgTyCon x then
            toBytes (TyCon.tyConRoles x)
         ++ uniqueBytes (TyCon.tyConStupidTheta x)
         ++ uniqueBytes (TyCon.algTyConRhs x)
         -- TODO: ++ uniqueBytes TyCon.algTyConParent??
         
          else if TyCon.isTypeSynonymTyCon x then
            toBytes (TyCon.tyConRoles x)
         ++ uniqueBytes (TyCon.tyConStupidTheta x)
         ++ uniqueBytesFromMaybe (TyCon.synTyConRhs_maybe x)

          else if TyCon.isFamilyTyCon x then
            toBytes (TyCon.tyConRoles x)
         ++ uniqueBytes (TyCon.tyConStupidTheta x)
         ++ uniqueBytesFromMaybe (TyCon.famTyConFlav_maybe x)

          else if TyCon.isPrimTyCon x then
            toBytes (TyCon.tyConRoles x)
         ++ uniqueBytes (TyCon.tyConStupidTheta x)
         ++ uniqueBytes (TyCon.isUnliftedTyCon x)

          else if TyCon.isPromotedDataCon x then
            toBytes (TyCon.tyConRoles x)
         ++ uniqueBytes (TyCon.tyConStupidTheta x)
         ++ uniqueBytesFromMaybe (TyCon.isPromotedDataCon_maybe x)
          else
        -- TODO: get bts
            []

        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

instance Hashable TyCon.TyConBndrVis where
    typeID (TyCon.NamedTCB _) = 0x00021000
    typeID (TyCon.AnonTCB _)  = 0x00021001

    uniqueBytes x = bytes where
        tb = typeID' x
        bts = case x of
          TyCon.NamedTCB y -> uniqueBytes y
          TyCon.AnonTCB y -> uniqueBytes y

        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

-- Type constructor right hand side of data
instance Hashable TyCon.AlgTyConRhs where
    typeID TyCon.AbstractTyCon = 0x00022000
    typeID TyCon.DataTyCon{}   = 0x00022001
    typeID TyCon.TupleTyCon{}  = 0x00022002
    typeID TyCon.SumTyCon{}    = 0x00022003
    typeID TyCon.NewTyCon{}    = 0x00022004

    uniqueBytes x = bytes where
        tb = typeID' x
        bts = case x of
          TyCon.DataTyCon{data_cons = data_cons} -> uniqueBytes data_cons
          TyCon.TupleTyCon{data_con = data_con, tup_sort = tup_sort} -> uniqueBytes data_con ++ toBytes tup_sort
          TyCon.SumTyCon{data_cons = data_cons} -> uniqueBytes data_cons
          TyCon.NewTyCon{data_con = data_con, nt_rhs = nt_rhs} -> uniqueBytes data_con ++ uniqueBytes nt_rhs
          _ -> []

        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

instance BinarySerializable BasicTypes.TupleSort where
    toBytes x = toBytes b where
      b = case x of
        BoxedTuple      -> 0x00 :: Word8
        UnboxedTuple    -> 0x01
        ConstraintTuple -> 0x02

instance Hashable TyCon.FamTyConFlav where
    typeID x = case x of
      TyCon.DataFamilyTyCon _            -> 0x00023000
      TyCon.OpenSynFamilyTyCon           -> 0x00023001
      TyCon.ClosedSynFamilyTyCon _       -> 0x00023002
      TyCon.AbstractClosedSynFamilyTyCon -> 0x00023003
      TyCon.BuiltInSynFamTyCon _         -> 0x00023004

    uniqueBytes x = bytes where
        tb = typeID' x
        bts = case x of
          TyCon.DataFamilyTyCon name   -> [] --TODO: get rid of name
          TyCon.ClosedSynFamilyTyCon m -> uniqueBytes m
          TyCon.BuiltInSynFamTyCon b   -> [] -- TODO: uniqueBytes b
          _ -> []

        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts
