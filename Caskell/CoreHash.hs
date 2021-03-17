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
import qualified Id
import qualified IdInfo
import qualified PatSyn
import qualified TcType
import qualified PrimOp
import qualified DataCon
import qualified ConLike
import qualified Class
import qualified HscTypes
import qualified NameEnv
import qualified TyCoRep
import qualified TyCon
import qualified Unique
import BasicTypes
import Outputable (Outputable, ppr, showSDocUnsafe)

import DynFlags
import CoAxiom

import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Functor
import Data.Coerce
import Data.Sort
import Data.Typeable
import Data.Word
import FastString

import Caskell.Bytes
import Caskell.Hash
import Caskell.PrimOpHash
import Caskell.Context
import Caskell.CoreCompare

showPpr' :: Outputable a => a -> String
showPpr' = showSDocUnsafe . ppr

--null_hash = error "null hash"
null_hash = get_hash ([]::[Int])
placeholder_hash = get_hash ([]::[Int])

name_filter :: String -> Bool
name_filter a = any (flip (isSuffixOf) a) ["$main", "$trModule", "$dIP"]

-- main hash function:
-- takes core data (Expr, Type, DataCon, etc), a filter function and a
-- function to obtain the hash from the coredata.
-- returns the existing hash if that core data has been hashed already
-- or hashes and inserts the data into the state if it has not been
-- hashed already.
hash_core_hashable :: CoreData -> (String -> Bool) -> CtxMonad (Hash) -> CtxMonad (Hash)
hash_core_hashable coredata name_filter hash_function = do
    let name = fromJust $ Caskell.Context.name $ hash_data coredata
    let sname = short_name name
    let uniq = Name.nameUnique name
    mexpr <- mlookup_unique uniq

    case mexpr of
        Just uh -> do
            let cd = fromJust $ Caskell.Context.lookup uniq $ hash_core_data uh
            if hole cd then do
                dprintln $ sname ++ " is RECURSIVE"
                return $ null_hash
            else do
                dprintln $ sname ++ " has been hashed already: " ++ (short_hash_str $ hash uh)
                return $ hash uh

        Nothing -> do
            let fullname = Name.nameStableString name

            if (not $ name_filter fullname) then do
                madd_hash coredata placeholder_hash

                hash <- hash_function
                dprintln ""

                mdelete_by_unique uniq

                let coredata' = coredata { hole = False }
                madd_hash coredata' hash

                return hash
            else do
                dprintln "filtered"
                return placeholder_hash

-- https://hackage.haskell.org/package/ghc-8.10.2/docs/src/GHC.html#CoreModule
hash_module :: GHC.CoreModule -> CtxMonad ()
hash_module guts@(GHC.CoreModule _ types binds _) = do
    set_mod_guts guts
    hash_types types
    hash_dataCons types -- datacons are also stored here
    hash_binds binds

-- types
-- https://hackage.haskell.org/package/ghc-8.10.2/docs/src/HscTypes.html#TypeEnv
hash_types :: HscTypes.TypeEnv -> CtxMonad ()
hash_types tenv = do
    mapM (hash_type_tything) $ HscTypes.typeEnvElts tenv
    return ()

-- https://hackage.haskell.org/package/ghc-8.10.2/docs/src/TyCoRep.html#TyThing
hash_type_tything :: TyCoRep.TyThing -> CtxMonad (Maybe Hash)
hash_type_tything (TyCoRep.ATyCon tc) = do
    ret <- hash_tyCon tc
    dprintln ""
    return $ Just ret
hash_type_tything (TyCoRep.ACoAxiom _) = return Nothing -- TODO: newtype
hash_type_tything (TyCoRep.AnId _) = return Nothing -- not a type
hash_type_tything (TyCoRep.AConLike _) = return Nothing -- data constructor, we want TYPES

-- https://hackage.haskell.org/package/ghc-8.10.2/docs/src/TyCon.html#TyCon
hash_tyCon :: TyCon.TyCon -> CtxMonad (Hash)
hash_tyCon tc = do
    let name = TyCon.tyConName tc
    let uniq = TyCon.tyConUnique tc
    let sname = short_name name

    let hash_func = \tc -> do
            dprint $ typeName tc ++ " " ++ sname ++ " = "
            
            let ret' | TyCon.isFunTyCon tc = hash_funTyCon tc
                     | TyCon.isAlgTyCon tc = hash_algTyCon tc
                     | TyCon.isPrimTyCon tc = hash_primTyCon tc
                     | TyCon.isPromotedDataCon tc = hash_promotedDataCon tc
                     -- TODO: implement the rest
                     | TyCon.isTypeSynonymTyCon tc = return null_hash
                     | TyCon.isFamilyTyCon tc = return null_hash
                     -- TyCon.TcTyCon
                     | True = return null_hash

            let tcid = typeID tc
            let bndrs = TyCon.tyConBinders tc
            let kind = TyCon.tyConResKind tc

            bndrs_hashes <- mapM hash_tyConBinder bndrs
            kind_hash <- hash_type kind
            content_hash <- ret'

            let tb = toBytes tcid
            let bndrs_bytes = map (toBytes) bndrs_hashes
            let kind_bytes = toBytes kind_hash
            let content_bytes = toBytes content_hash

            let hsh = get_hash $ tb : bndrs_bytes ++ kind_bytes : [content_bytes]
            return hsh

    let hash_data = TyCon tc
    let coredata = coreData uniq hash_data True

    hash_core_hashable coredata name_filter (hash_func tc)

hash_tyConBinder :: TyCon.TyConBinder -> CtxMonad (Hash)
hash_tyConBinder (Var.Bndr v vis) = do
    vhash <- hash_var v
    let visb = uniqueBytes vis

    return $ get_hash (visb ++ toBytes vhash)

-- https://hackage.haskell.org/package/ghc-8.10.2/docs/src/TyCon.html#FunTyCon
hash_funTyCon :: TyCon.TyCon -> CtxMonad (Hash)
hash_funTyCon tc = do
    let tcid = typeID tc
    -- TODO: implement
    return null_hash

-- https://hackage.haskell.org/package/ghc-8.10.2/docs/src/TyCon.html#AlgTyCon
hash_algTyCon :: TyCon.TyCon -> CtxMonad (Hash)
hash_algTyCon tc = do
    -- TODO: stupid theta (the things on the left side, e.g. data Eq a => T a
    let rhs = TyCon.algTyConRhs tc
    let rhsid = typeID rhs

    dprint $ typeName rhs

    rhs_bts <- case rhs of
      TyCon.AbstractTyCon -> return []
      TyCon.DataTyCon dcs _ _ -> do
        args_hash <- hash_dataCons_args dcs
        return $ toBytes args_hash

      TyCon.TupleTyCon dc sort -> return $ toBytes sort -- TODO
      TyCon.SumTyCon dcs _ -> return [] -- TODO
      TyCon.NewTyCon dc t _ _ _ -> return [] -- TODO

    let bts = toBytes rhsid ++ rhs_bts
    return $ get_hash bts

-- this gets the type, order and number of data constructor arguments
-- for each data constructor in the Data type and uniquely hashes them
hash_dataCons_args :: [DataCon.DataCon] -> CtxMonad (Hash)
hash_dataCons_args dcs' = do
    -- we sort to get stable and order independent hashes for type constructors
    let dcs = order_dataCons dcs'

    let hash_dataCon_args dc = do
            let dc_name = short_name $ DataCon.dataConName dc

            let sig@(univ, ex, _, _, args, ret) = DataCon.dataConFullSig dc
            let ret_tyargs = case ret of
                                -- we do this to get the type arguments for the
                                -- return value of the data constructor
                                -- (the resulting type).
                                -- we can't hash the resulting type here because
                                -- that would cause infinite recursion.
                                TyCoRep.TyConApp _ args -> args
                                _ -> []

            dprint $ "<" ++ dc_name ++ "("

            --tyvar_hashes <- mapM hash_var dcuniv
            -- dprintln $ concatMap show tyvar_hashes
            tyvar_hashes <- mapM hash_var univ
            extyvar_hashes <- mapM hash_var ex
            args_hashes <- mapM hash_type args
            ret_tyargs_hashes <- mapM hash_type ret_tyargs

            dprint ")>"
            let tyvar_bytes = map (toBytes) tyvar_hashes
            let extyvar_bytes = map (toBytes) extyvar_hashes
            let args_bytes = map (toBytes) args_hashes
            let ret_tyargs_bytes = map (toBytes) ret_tyargs_hashes

            return $ get_hash $ tyvar_bytes ++ extyvar_bytes ++ args_bytes ++ ret_tyargs_bytes

    dc_hashes <- mapM hash_dataCon_args dcs
    let dc_bytes = map (toBytes) dc_hashes
    return $ get_hash dc_bytes

-- https://hackage.haskell.org/package/ghc-8.10.2/docs/src/TyCon.html#PrimTyCon
hash_primTyCon :: TyCon.TyCon -> CtxMonad (Hash)
hash_primTyCon tc = do
    -- prim tycons are, well, primitives.
    -- just hash them by name
    let tcname = TyCon.tyConName tc

    return $ get_hash $ Name.nameStableString tcname

-- https://hackage.haskell.org/package/ghc-8.10.2/docs/src/TyCon.html#PromotedDataCon
hash_promotedDataCon :: TyCon.TyCon -> CtxMonad (Hash)
hash_promotedDataCon tc = do
    --let tcname = TyCon.tyConName tc
    return $ null_hash

-- type hash
-- https://hackage.haskell.org/package/ghc-8.10.2/docs/src/TyCoRep.html#Type
hash_type :: TyCoRep.Type -> CtxMonad (Hash)
hash_type t = do
    let tid = typeID t
    let tname = typeName t

    dprint $ tname ++ "("

    hob <- case t of
        TyCoRep.TyVarTy var -> do
            vhash <- hash_var var
            return $ H vhash
            -- TODO: finish the rest
        TyCoRep.AppTy t1 t2 -> return $ B tid []
        TyCoRep.TyConApp tc ts -> do
            tchash <- hash_tyCon tc
            dprint "["
            typehashes <- mapM (hash_type) ts
            dprint "]"
            return $ B tid $ toBytes tchash ++ concatMap (toBytes) typehashes
        TyCoRep.ForAllTy _ _ -> return $ B tid []
        TyCoRep.FunTy{} -> return $ B tid []
        TyCoRep.LitTy _ -> return $ B tid []
        TyCoRep.CastTy _ _ -> return $ B tid []
        TyCoRep.CoercionTy _ -> return $ B tid []

    dprint ")"

    return $ get_hash hob

-- data cons
hash_dataCons :: HscTypes.TypeEnv -> CtxMonad ()
hash_dataCons tenv = do
    mapM (hash_dataCon_tything) $ HscTypes.typeEnvElts tenv
    return ()

-- https://hackage.haskell.org/package/ghc-8.10.2/docs/src/TyCoRep.html#TyThing
hash_dataCon_tything :: TyCoRep.TyThing -> CtxMonad (Maybe Hash)
hash_dataCon_tything (TyCoRep.AConLike ac) = do
    case ac of
      ConLike.RealDataCon dc -> do
        hs <- hash_dataCon dc
        return $ Just hs
      _ -> return Nothing
hash_dataCon_tything _ = return Nothing
                 
-- stable order
order_dataCons :: [DataCon.DataCon] -> [DataCon.DataCon]
order_dataCons = sort

dataCon_parent_tycon_dataCons :: DataCon.DataCon -> [DataCon.DataCon]
dataCon_parent_tycon_dataCons dc = dcs where
    ty = DataCon.dataConOrigResTy dc
    tycon = case ty of
                TyCoRep.TyConApp tc _ -> tc
                _ -> error "not a tycon"

    dcs = TyCon.tyConDataCons tycon

dataCon_stable_tag :: DataCon.DataCon -> Int
dataCon_stable_tag dc = t where
    dcs = dataCon_parent_tycon_dataCons dc
    dcs' = order_dataCons dcs
    t = fromJust $ findIndex (==dc) dcs'

hash_dataCon :: DataCon.DataCon -> CtxMonad (Hash)
hash_dataCon dc = do
    let dc_name = short_name $ DataCon.dataConName dc
    let uniq = Name.nameUnique $ DataCon.dataConName dc

    let hash_func =
          \dc -> do
            -- TODO: univ, ex
            let sig@(univ, ex, _, _, args, ret) = DataCon.dataConFullSig dc
            dprint $ "<" ++ dc_name ++ "("

            --tyvar_hashes <- mapM hash_var dcuniv
            args_hashes <- mapM hash_type args
            ret_hash <- hash_type ret
            let stable_tag = dataCon_stable_tag dc

            dprint ")>"
            let args_bytes = map (toBytes) args_hashes
            let ret_bytes = toBytes ret_hash
            let tag_bytes = uniqueBytes stable_tag

            return $ get_hash $ args_bytes ++ [ret_bytes, tag_bytes]

    let hashdata = DataCon dc
    let coredata = coreData uniq hashdata True

    hash_core_hashable coredata name_filter (hash_func dc)


-- normal expressions
hash_binds :: CoreSyn.CoreProgram -> CtxMonad ()
hash_binds binds = do
    mapM (hash_bind) binds
    return ()

hash_bind :: CoreSyn.CoreBind -> CtxMonad (Hash)
hash_bind cb@(CoreSyn.NonRec b expr) = do
    let name = Var.varName b
    let uniq = Name.nameUnique name

    let hash_data = CoreBind cb
    let coredata = coreData uniq hash_data True

    hash_core_hashable coredata name_filter (hash_expr expr)

hash_bind (CoreSyn.Rec l) = undefined -- TODO: implement

hash_expr :: CoreSyn.Expr b -> CtxMonad (Hash)
hash_expr expr = do
    let tid = typeID expr
    let tname = typeName expr

    dprint tname

    hob <- case expr of
        CoreSyn.Var var -> do
            dprint "."
            h <- hash_var var
            return $ H h

        CoreSyn.Lit lit -> do
            dprint "."
            h <- hash_literal lit
            return $ H h

        CoreSyn.App e1 e2 -> do
            dprint "("
            b1 <- hash_expr e1
            dprint ", "
            b2 <- hash_expr e2
            dprint ")"
            return $ B tid $ toBytes b1 ++ toBytes b2

    -- TODO: implement
        CoreSyn.Lam b e -> do
            return $ B tid []

    -- TODO: implement
        CoreSyn.Let b e -> do
            return $ B tid []

    -- TODO: implement
        CoreSyn.Case e b t alts -> do
            return $ B tid []

    -- TODO: implement
        CoreSyn.Cast e coer -> do
            return $ B tid []

    -- TODO: implement
        CoreSyn.Type t -> do
            return $ B tid []

    -- TODO: implement
        CoreSyn.Coercion coer -> do
            return $ B tid []

        _ -> return $ B tid []

    let hash = get_hash hob

    return hash

hash_var :: Var.Var -> CtxMonad (Hash)
hash_var var = do
    let tname = typeName var
    let vname = Var.varName var
    let uniq = Name.nameUnique vname

    dprint $ tname ++ "(" ++ short_name vname ++ ")"

    -- Find in already hashed values -> return hash
    mexpr <- mlookup_unique uniq

    case mexpr of
      Just uh -> return $ hash uh
      Nothing -> do
      -- Find in module -> hash -> return hash
        mmodexpr <- mlookup_unique_in_module uniq
        
        case mmodexpr of
          Just bind -> do
            h <- hash_bind bind
            -- add the existing hash to the var
            uh' <- mlookup_hash h
            let uh = fromJust uh'
            let ref = hash_data_ref uh
            madd_hash (CoreData uniq (Var var) False ref False) h
            return h

          Nothing -> do
            if (Var.isId var) then do
                mh <- hash_id var
                case mh of
                  Just (h, defless) -> do
                    if not defless then do
                        uh' <- mlookup_hash h
                        let uh = fromJust uh'
                        let ref = hash_data_ref uh
                        madd_hash (CoreData uniq (Var var) False ref False) h
                    else
                        madd_hash (CoreData uniq (Var var) False Nothing True) h
                        
                    return h

                  Nothing -> do
                    dprint "[var not found]"
                    return null_hash

            else if (Var.isTyVar var) then do
                h <- hash_tyVar var
                return h

            else do
                dprint "[var not found]"
                return null_hash

-- specialization of hash_var that was not found, probably extern
hash_id :: Var.Var -> CtxMonad (Maybe (Hash, Bool))
hash_id var = do
    let vname = Var.varName var
    let uniq = Name.nameUnique vname
    let details = Var.idDetails var

    case details of
      IdInfo.DataConWorkId dc -> do
        h <- hash_dataCon dc
        return $ Just (h, False)

      IdInfo.DataConWrapId dc -> do
        h <- hash_dataCon dc
        return $ Just (h, False)
        
        -- TODO: class
      _ -> do
        let unfold = Id.idUnfolding var

        case unfold of
          CoreSyn.NoUnfolding -> do
            dprintln "[no definition available]"
            return $ Just (get_hash $ Name.nameStableString vname, True)
          _ -> do
            dprintln "UNFOLDING?"
            return $ Just (null_hash, True)

hash_tyVar :: Var.Var -> CtxMonad (Hash)
hash_tyVar var = do
    let t = Var.varType var
    hash_type t
    

hash_literal :: Literal.Literal -> CtxMonad (Hash)
hash_literal lit = do
    let tid = typeID lit
    let tname = typeName lit

    dprint tname

    hob <- case lit of
        Literal.LitChar c -> do
            dprint $ "(" ++ c : ")"
            return $ B tid $ uniqueBytes c

        -- TODO: number type maybe
        Literal.LitNumber lnt i t -> do
            dprint $ "(" ++ show i ++ ")"
            return $ B tid $ toBytes lnt ++ uniqueBytes i

        Literal.LitString s -> do
            dprint $ "(" ++ show s ++ ")"
            return $ B tid $ uniqueBytes s

        Literal.LitFloat r -> do
            dprint $ "(" ++ show r ++ ")"
            return $ B tid $ uniqueBytes r

        Literal.LitDouble r -> do
            dprint $ "(" ++ show r ++ ")"
            return $ B tid $ uniqueBytes r
        _ -> do
            return $ B tid []

    let hash = get_hash hob

    return hash

-- ========
-- TYPE IDS
-- ========
instance TypeIDAble (CoreSyn.Expr b) where
    typeID x = case x of
        CoreSyn.Var _        -> 0x00001000
        CoreSyn.Lit _        -> 0x00001001
        CoreSyn.App _ _      -> 0x00001002
        CoreSyn.Lam _ _      -> 0x00001003
        CoreSyn.Let _ _      -> 0x00001004
        CoreSyn.Case _ _ _ _ -> 0x00001005
        CoreSyn.Cast _ _     -> 0x00001006
        CoreSyn.Tick _ _     -> 0x00001007
        CoreSyn.Type _       -> 0x00001008

    typeName (CoreSyn.Var _)          = "Var"
    typeName (CoreSyn.Lit _)          = "Literal"
    typeName (CoreSyn.App _ _)        = "Application"
    typeName (CoreSyn.Lam _ _)        = "Lambda"
    typeName (CoreSyn.Let _ _)        = "Let"
    typeName (CoreSyn.Case _ _ _ _)   = "Case"
    typeName (CoreSyn.Cast _ _)       = "Cast"
    typeName (CoreSyn.Tick _ _)       = "Tick"
    typeName (CoreSyn.Type _)         = "Type"

instance TypeIDAble Literal.Literal where
    typeID (Literal.LitChar _)          = 0x00002000
    typeID (Literal.LitNumber _ _ _)    = 0x00002001
    typeID (Literal.LitString _)        = 0x00002002
    typeID (Literal.LitNullAddr)        = 0x00002003
    typeID (Literal.LitRubbish)         = 0x00002004
    typeID (Literal.LitFloat _)         = 0x00002005
    typeID (Literal.LitDouble _)        = 0x00002006
    typeID (Literal.LitLabel _ _ _)     = 0x00002007

    typeName (Literal.LitChar _)          = "LitChar"
    typeName (Literal.LitNumber _ _ _)    = "LitNumber"
    typeName (Literal.LitString _)        = "LitString"
    typeName (Literal.LitNullAddr)        = "LitNullAddr"
    typeName (Literal.LitRubbish)         = "LitRubbish"
    typeName (Literal.LitFloat _)         = "LitFloat"
    typeName (Literal.LitDouble _)        = "LitDouble"
    typeName (Literal.LitLabel _ _ _)     = "LitLabel"

instance TypeIDAble TyCoRep.Type where
    typeID (TyCoRep.TyVarTy _)    = 0x00003000
    typeID (TyCoRep.AppTy _ _)    = 0x00003001
    typeID (TyCoRep.TyConApp _ _) = 0x00003002
    typeID (TyCoRep.ForAllTy _ _) = 0x00003003
    typeID (TyCoRep.FunTy{})      = 0x00003004
    typeID (TyCoRep.LitTy _)      = 0x00003005
    typeID (TyCoRep.CastTy _ _)   = 0x00003006
    typeID (TyCoRep.CoercionTy _) = 0x00003007

    typeName (TyCoRep.TyVarTy _)    = "TyVarTy"
    typeName (TyCoRep.AppTy _ _)    = "AppTy"
    typeName (TyCoRep.TyConApp _ _) = "TyConApp"
    typeName (TyCoRep.ForAllTy _ _) = "ForAllTy"
    typeName (TyCoRep.FunTy{})      = "FunTy"
    typeName (TyCoRep.LitTy _)      = "LitTy"
    typeName (TyCoRep.CastTy _ _)   = "CastTy"
    typeName (TyCoRep.CoercionTy _) = "CoercionTy"

instance TypeIDAble Var.Var where
    typeID x
        | Var.isTyVar x   = 0x00004000
        | Var.isTcTyVar x = 0x00004001
        | Var.isId x      = 0x00004002
        | True            = 0x00004003

    typeName x
        | Var.isTyVar x   = "TyVar"
        | Var.isTcTyVar x = "TcTyVar"
        | Var.isId x      = "Id"
        | True            = ""

instance TypeIDAble (Var.VarBndr a b) where
    typeID = const 0x00004004

instance TypeIDAble TyCoRep.TyLit where
    typeID (TyCoRep.NumTyLit _) = 0x00005000
    typeID (TyCoRep.StrTyLit _) = 0x00005001

instance TypeIDAble TyCoRep.Coercion where
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

instance TypeIDAble TyCoRep.MCoercion where
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

    typeName x = case x of
      IdInfo.VanillaId       -> "VanillaId"
      IdInfo.RecSelId{}      -> "RecSelId"   
      IdInfo.DataConWorkId _ -> "DataConWorkId"
      IdInfo.DataConWrapId _ -> "DataConWrapId"
      IdInfo.ClassOpId _     -> "ClassOpId"
      IdInfo.PrimOpId _      -> "PrimOpId"   
      IdInfo.FCallId _       -> "FCallId"    
      IdInfo.TickBoxOpId _   -> "TickBoxOpId"
      IdInfo.DFunId _        -> "DFunId" 
      IdInfo.CoVarId         -> "CoVarId"      
      IdInfo.JoinId _        -> "JoinId"     


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
    typeID x
        | TyCon.isFunTyCon x =         0x00020000
        | TyCon.isAlgTyCon x =         0x00020001
        | TyCon.isTypeSynonymTyCon x = 0x00020002
        | TyCon.isFamilyTyCon x =      0x00020003
        | TyCon.isPrimTyCon x =        0x00020004
        | TyCon.isPromotedDataCon x =  0x00020005
        -- only used during typechecking
        -- | TyCon.isTcTyCon x =          0x00020006
        | True = 0x00020006

    typeName x
        | TyCon.isFunTyCon x =         "FunTyCon"
        | TyCon.isAlgTyCon x =         "AlgTyCon"
        | TyCon.isTypeSynonymTyCon x = "TypeSynonymTyCon"
        | TyCon.isFamilyTyCon x =      "FamilyTyCon"
        | TyCon.isPrimTyCon x =        "PrimTyCon"
        | TyCon.isPromotedDataCon x =  "PromotedDataCon"
        -- only used during typechecking
        -- | TyCon.isTcTyCon x =          0x00020006
        | True = "Other TyCon"


instance TypeIDAble TyCon.TyConBndrVis where
    typeID (TyCon.NamedTCB _) = 0x00021000
    typeID (TyCon.AnonTCB _)  = 0x00021001
    typeName (TyCon.NamedTCB _) = "Named"
    typeName (TyCon.AnonTCB _)  = "Anon"

instance UniquelySerializable TyCon.TyConBndrVis where
    uniqueBytes x = bytes where
        tb = toBytes $ typeID x
        bts = case x of
          TyCon.NamedTCB y -> toBytes y
          TyCon.AnonTCB y -> toBytes y

        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (bytesLength lb :: Word8)) ++ lb ++ bts

-- Type constructor right hand side of data
instance TypeIDAble TyCon.AlgTyConRhs where
    typeID TyCon.AbstractTyCon = 0x00022000
    typeID TyCon.DataTyCon{}   = 0x00022001
    typeID TyCon.TupleTyCon{}  = 0x00022002
    typeID TyCon.SumTyCon{}    = 0x00022003
    typeID TyCon.NewTyCon{}    = 0x00022004

    typeName TyCon.AbstractTyCon = "AbstractTyCon"
    typeName TyCon.DataTyCon{}   = "DataTyCon"
    typeName TyCon.TupleTyCon{}  = "TupleTyCon"
    typeName TyCon.SumTyCon{}    = "SumTyCon"
    typeName TyCon.NewTyCon{}    = "NewTyCon"

instance TypeIDAble TyCon.FamTyConFlav where
    typeID x = case x of
      TyCon.DataFamilyTyCon _            -> 0x00023000
      TyCon.OpenSynFamilyTyCon           -> 0x00023001
      TyCon.ClosedSynFamilyTyCon _       -> 0x00023002
      TyCon.AbstractClosedSynFamilyTyCon -> 0x00023003
      TyCon.BuiltInSynFamTyCon _         -> 0x00023004

instance TypeIDAble TyCon.AlgTyConFlav where
    typeID x = case x of
      TyCon.VanillaAlgTyCon _      -> 0x00024000
      TyCon.UnboxedAlgTyCon _      -> 0x00024001
      TyCon.ClassTyCon _ _         -> 0x00024002
      TyCon.DataFamInstTyCon _ _ _ -> 0x00024003

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

instance BinarySerializable TyCoRep.ArgFlag where
    toBytes TyCoRep.Inferred  = toBytes (0x00 :: Word8)
    toBytes TyCoRep.Specified = toBytes (0x01 :: Word8)
    toBytes TyCoRep.Required  = toBytes (0x02 :: Word8)

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

