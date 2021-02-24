{-|
    this module uses the Hash module to hash a GHC Core Module

    https://downloads.haskell.org/~ghc/8.8.3/docs/html/libraries/ghc-8.8.3/CoreSyn.html#t:Expr
|-}

module Caskell.CoreHash
(
    uniqueBytes,
    hash_module
) where

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified GHC
import qualified Name
import qualified CoreSyn
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

import Control.Monad
import Control.Monad.State
import Data.List
import Data.Functor
import Data.Coerce
import Data.Typeable
import Data.Word
import FastString
import Unique

import Caskell.Bytes
import Caskell.Hash
import Caskell.PrimOpHash
import Caskell.Context

null_hash = get_hash ([]::[Int])

typeID' :: Hashable a => a -> [BS.ByteString]
typeID' = toBytes . typeID 

hash_module :: GHC.CoreModule -> CtxMonad ()
hash_module (GHC.CoreModule _ _ binds _) = hash_binds binds

hash_binds :: CoreSyn.CoreProgram -> CtxMonad ()
hash_binds binds = do
    mapM (hash_bind) binds
    return ()

hash_bind :: CoreSyn.CoreBind -> CtxMonad ()
hash_bind cb@(CoreSyn.NonRec b expr) = do
    let name = Var.varName b
    let uniq = Name.nameUnique name
    let hash_data = CoreBind cb

    let fullname = Name.nameStableString name
    let name_filter = any (flip (isSuffixOf) fullname) ["$main", "$trModule", "$dIP"]

    let sname = short_name name

    when (not name_filter) $ do
    
        insert_hash $ UniqueHash uniq hash_data null_hash True

        lift $ putStr $ sname ++ " = "
        bytes <- hash_bytes_expr expr
        let hash = get_hash bytes
        lift $ putStrLn ""

        insert_hash $ UniqueHash uniq hash_data hash False

hash_bind (CoreSyn.Rec l) = undefined

hash_bytes_expr :: CoreSyn.Expr b -> CtxMonad (Bytes)
hash_bytes_expr expr = do

    bytes <- case expr of
    -- TODO: implement
        CoreSyn.Var var -> do
            let t = Var.varType var
            lift $ putStr $ "var(" ++ short_name (Var.varName var) ++ ")"
            return []

        CoreSyn.Lit lit -> do
            lift $ putStr "literal"
            return $ [] -- uniqueBytes lit

        CoreSyn.App e1 e2 -> do
            lift $ putStr "app("
            b1 <- hash_bytes_expr e1
            lift $ putStr ", "
            b2 <- hash_bytes_expr e2
            lift $ putStr ")"
            return $ b1 ++ b2

        CoreSyn.Lam b e -> do
            lift $ putStr "lambda"
            return []

        CoreSyn.Let b e -> do
            lift $ putStr "let"
            return []

        CoreSyn.Case e b t alts -> do
            lift $ putStr "case"
            return []

        CoreSyn.Cast e coer -> do
            lift $ putStr "cast"
            return []

        CoreSyn.Type t -> do
            lift $ putStr "type"
            return []

        CoreSyn.Coercion coer -> do
            lift $ putStr "coercion"
            return []

        _ -> return []

    return bytes

instance TypeIDAble (CoreSyn.Expr b) where
    typeID (CoreSyn.Var _)          = 0x00001000
    typeID (CoreSyn.Lit _)          = 0x00001001
    typeID (CoreSyn.App _ _)        = 0x00001002
    typeID (CoreSyn.Lam _ _)        = 0x00001003
    typeID (CoreSyn.Let _ _)        = 0x00001004
    typeID (CoreSyn.Case _ _ _ _)   = 0x00001005
    typeID (CoreSyn.Cast _ _)       = 0x00001006
    typeID (CoreSyn.Tick _ _)       = 0x00001007
    typeID (CoreSyn.Type _)         = 0x00001008

instance TypeIDAble Literal.Literal where
    typeID (Literal.LitChar _)          = 0x00002000
    typeID (Literal.LitNumber _ _ _)    = 0x00002001
    typeID (Literal.LitString _)        = 0x00002002
    typeID (Literal.LitNullAddr)        = 0x00002003
    typeID (Literal.LitRubbish)         = 0x00002004
    typeID (Literal.LitFloat _)         = 0x00002005
    typeID (Literal.LitDouble _)        = 0x00002006
    typeID (Literal.LitLabel _ _ _)     = 0x00002007

instance TypeIDAble Type where
    typeID (TyVarTy _)    = 0x00003000
    typeID (AppTy _ _)    = 0x00003001
    typeID (TyConApp _ _) = 0x00003002
    typeID (ForAllTy _ _) = 0x00003003
    typeID (FunTy{})      = 0x00003004
    typeID (LitTy _)      = 0x00003005
    typeID (CastTy _ _)   = 0x00003006
    typeID (CoercionTy _) = 0x00003007

instance TypeIDAble Var.Var where
    typeID x
        | Var.isTyVar x   = 0x00004000
        | Var.isTcTyVar x = 0x00004001
        | Var.isId x      = 0x00004002
        | True            = 0x00004003

instance TypeIDAble (Var.VarBndr a b) where
    typeID = const 0x00004004

instance TypeIDAble TyLit where
    typeID (NumTyLit _) = 0x00005000
    typeID (StrTyLit _) = 0x00005001

instance TypeIDAble Coercion where
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

instance TypeIDAble MCoercion where
    typeID x = case x of
      TyCoRep.MRefl -> 0x00007000
      TyCoRep.MCo _ -> 0x00007001

instance TypeIDAble (CoAxiom.CoAxiom br) where
    typeID = const 0x00008000

-- IGNORE NAMES
instance TypeIDAble Name.Name where
    typeID = const 0x00009000

instance TypeIDAble (Branches br) where
    typeID = const 0x0000A000

instance TypeIDAble CoAxBranch where
    typeID = const 0x0000B000

instance TypeIDAble CoAxiomRule where
    typeID = const 0x0000C000

instance TypeIDAble TyCoRep.UnivCoProvenance where
    typeID x = case x of
      TyCoRep.UnsafeCoerceProv -> 0x0000D000
      TyCoRep.PhantomProv _    -> 0x0000D001
      TyCoRep.ProofIrrelProv _ -> 0x0000D002
      TyCoRep.PluginProv _     -> 0x0000D003
    
instance TypeIDAble IdInfo.IdDetails where
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

instance TypeIDAble IdInfo.RecSelParent where
    typeID x = case x of
      IdInfo.RecSelData _   -> 0x0000F000
      IdInfo.RecSelPatSyn _ -> 0x0000F001

instance TypeIDAble PatSyn.PatSyn where
    typeID = const 0x00010000


instance TypeIDAble TcType.TcTyVarDetails where
    typeID x = case x of
      TcType.SkolemTv _ _   -> 0x00011000
      TcType.RuntimeUnk     -> 0x00011001
      TcType.MetaTv{}       -> 0x00011002

-- DataCon.DataCon
-- Data Constructor
-- https://hackage.haskell.org/package/ghc-8.10.2/docs/DataCon.html#g:1
instance TypeIDAble DataCon.DataCon where
    typeID = const 0x00012000

-- Class.Class
-- TypeClass
instance TypeIDAble Class.Class where
    typeID = const 0x00013000

instance TypeIDAble (DefMethSpec ty) where
    typeID x = case x of
      VanillaDM   -> 0x00014000
      GenericDM _ -> 0x00014001

-- 0x00015xxx is used by PrimOp
instance TypeIDAble (CoreSyn.Bind a) where
    typeID (CoreSyn.NonRec _ _) = 0x00015F00
    typeID (CoreSyn.Rec _)      = 0x00015F01

instance TypeIDAble CoreSyn.AltCon where
    typeID x = case x of
      CoreSyn.DataAlt _ -> 0x00016000
      CoreSyn.LitAlt _  -> 0x00016001
      CoreSyn.DEFAULT   -> 0x00016002

-- TyCon
instance TypeIDAble TyCon.TyCon where
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

instance TypeIDAble TyCon.TyConBndrVis where
    typeID (TyCon.NamedTCB _) = 0x00021000
    typeID (TyCon.AnonTCB _)  = 0x00021001

-- Type constructor right hand side of data
instance TypeIDAble TyCon.AlgTyConRhs where
    typeID TyCon.AbstractTyCon = 0x00022000
    typeID TyCon.DataTyCon{}   = 0x00022001
    typeID TyCon.TupleTyCon{}  = 0x00022002
    typeID TyCon.SumTyCon{}    = 0x00022003
    typeID TyCon.NewTyCon{}    = 0x00022004

instance TypeIDAble TyCon.FamTyConFlav where
    typeID x = case x of
      TyCon.DataFamilyTyCon _            -> 0x00023000
      TyCon.OpenSynFamilyTyCon           -> 0x00023001
      TyCon.ClosedSynFamilyTyCon _       -> 0x00023002
      TyCon.AbstractClosedSynFamilyTyCon -> 0x00023003
      TyCon.BuiltInSynFamTyCon _         -> 0x00023004

-- =========================
-- Binary Serializable stuff
-- =========================
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

instance BinarySerializable ArgFlag where
    toBytes Inferred  = toBytes (0x00 :: Word8)
    toBytes Specified = toBytes (0x01 :: Word8)
    toBytes Required  = toBytes (0x02 :: Word8)

instance BinarySerializable Var.AnonArgFlag where
    toBytes Var.VisArg   = toBytes (0x00 :: Word8)
    toBytes Var.InvisArg = toBytes (0x01 :: Word8)

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

instance BinarySerializable TcType.TcLevel where
    toBytes (TcType.TcLevel x) = toBytes x

instance BinarySerializable TcType.MetaInfo where
    toBytes x = toBytes b where
        b = case x of
          TcType.TauTv      -> 0x00 :: Word8
          TcType.TyVarTv    -> 0x01 :: Word8
          TcType.FlatMetaTv -> 0x02 :: Word8
          TcType.FlatSkolTv -> 0x03 :: Word8

instance BinarySerializable BasicTypes.TupleSort where
    toBytes x = toBytes b where
      b = case x of
        BoxedTuple      -> 0x00 :: Word8
        UnboxedTuple    -> 0x01
        ConstraintTuple -> 0x02

