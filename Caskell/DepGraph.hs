{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

{- 
 - this module contains the dependency graph needed to hash recursive
 - GHC Core expressions and types
 -}

module Caskell.DepGraph
(
    DepGraph(..),

    TyDepGraph,
    DepGraphTypeRecord(..),
    DepDataConRecordEntry(..),
    RecTy(..),

    emptyTyDepGraph,
    dep_add_tyCon,
    dep_graph_from_tyCon
) where

import qualified Name
import qualified Unique
import qualified DataCon
import qualified ConLike
import qualified TyCon
import qualified TyCoRep
import qualified GHC
import qualified Var

import Debug.Trace
import Control.Monad.State
import Data.List
import Data.List.Split
import Data.Maybe

import Caskell.Utility

data (Eq a) => DepGraph a = DepGraph
    { dep_records :: [a]
    }

data DepGraphTypeRecord = DepGraphTypeRecord
    { tyr_tyCon :: TyCon.TyCon
    , tyr_dataCons :: [DepDataConRecordEntry]
    } deriving (Eq)

mkDepGraphTypeRecord tc = DepGraphTypeRecord
    { tyr_tyCon = tc
    , tyr_dataCons = []
    }

data DepDataConRecordEntry = DepDataConRecordEntry
    { dce_dataCon :: DataCon.DataCon
    , dce_args :: [RecTy]
    }

mkDepDataConRecordEntry dc = DepDataConRecordEntry
    { dce_dataCon = dc
    , dce_args = []
    }

instance Eq DepDataConRecordEntry where
    (==) a b = dce_dataCon a == dce_dataCon b

data RecTy = Tc TyCon.TyCon [RecTy] -- arguments
             | FunTy [RecTy] -- always exactly 2
             | Rec Int -- int = index in dep_records, ONLY for recursive tycons

recTy_constructor_order :: RecTy -> Int
recTy_constructor_order (Tc _ _) = 0
recTy_constructor_order (FunTy _) = 1
recTy_constructor_order (Rec _) = 2

instance Show RecTy where
    -- ONLY USE FOR DEBUG
    show r = case r of
            Tc tc args -> s' where
                ts = short_name $ TyCon.tyConName tc 
                argss = intercalate " " $ map (show) args
                s' = if null argss then ts else ts ++ " (" ++ argss ++ ")"

            FunTy l -> show (l !! 0) ++ " -> " ++ show (l !! 1)

            Rec i -> ">>" ++ show i


type TyDepGraph = DepGraph DepGraphTypeRecord

emptyTyDepGraph :: TyDepGraph
emptyTyDepGraph = DepGraph {
    dep_records = []
}

instance Show TyDepGraph where
    show g = s where
      tyr = dep_records g
      recs = \i -> ">>" ++ (tcs $ tyr_tyCon $ tyr !! i)

      dcs = short_name . DataCon.dataConName
      tcs = short_name . TyCon.tyConName

      -- show one recty
      rectys r = case r of
                Tc tc args -> s' where
                    ts = short_name $ TyCon.tyConName tc 
                    argss = intercalate " " $ map (rectys) args
                    s' = if null argss then ts else ts ++ " (" ++ argss ++ ")"
                
                FunTy l -> rectys (l !! 0) ++ " -> " ++ rectys (l !! 1)
                Rec i -> recs i
            

      -- show one DepDataConRecordEntry
      dcres dcre = s' where
            ds = dcs $ dce_dataCon dcre
            argss = intercalate " " $ map (rectys) (dce_args dcre)
            s' = ds ++ "(" ++ argss ++ ")"

      -- show one record
      rs r = s' where
            ts = tcs $ tyr_tyCon r
            dcss = intercalate ", " $ map (dcres) $ tyr_dataCons r
            s' = ts ++ "<" ++ dcss ++ ">"
            
      records = map rs tyr
      s = "[ " ++ intercalate "\n, " records ++ "\n]"

instance Show TyCoRep.Type where
    show t = case t of
        TyCoRep.TyVarTy var -> "~" ++ (short_name $ Var.varName var)
        TyCoRep.AppTy t1 t2 -> concat ["(", show t1, " ", show t2, ")"]

        TyCoRep.TyConApp tc ts -> s where
            tcname = "+" ++ (short_name $ TyCon.tyConName tc)
            args' = intercalate " " $ map (show) ts
            args = if null args' then "" else " " ++ args'
            s = "(" ++ tcname ++ args ++ ")"

        TyCoRep.FunTy _ t arg -> concat [show t, " -> ", show arg]

        TyCoRep.ForAllTy _ _ -> ""
        TyCoRep.LitTy _ -> ""
        TyCoRep.CastTy _ _ -> ""
        TyCoRep.CoercionTy _ -> ""

-- functions
get_record :: (Eq a) => DepGraph a -> Int -> a
get_record graph index = (dep_records graph !! index)

record_index :: (Eq a) => DepGraph a -> a -> Maybe Int
record_index graph r = elemIndex r (dep_records graph)

tyConRec_index :: TyDepGraph -> TyCon.TyCon -> Maybe Int
tyConRec_index graph tc = findIndex_s (==tc) (tyr_tyCon) (dep_records graph)

dataConRec_index :: TyDepGraph -> DataCon.DataCon -> Maybe (Int, Int)
dataConRec_index graph dc = do
    let tc = DataCon.dataConTyCon dc
    i1 <- tyConRec_index graph tc
    let tcr = get_record graph i1
    let dcs = tyr_dataCons tcr

    i2 <- findIndex_s (==dc) (dce_dataCon) dcs
    return (i1, i2)

-- helper stuff

type DepMonad g a = State (DepGraph g) a
type TyDepMonad a = DepMonad DepGraphTypeRecord a

mget_tyConRec :: Int -> TyDepMonad (DepGraphTypeRecord)
mget_tyConRec index = do
    graph <- get
    return $ get_record graph index

mget_dataConRec :: (Int, Int) -> TyDepMonad (DepDataConRecordEntry)
mget_dataConRec is = do
    (_, dr) <- mget_tyConRec_dataConRec is
    return dr

mget_tyConRec_dataConRec :: (Int, Int) -> TyDepMonad (DepGraphTypeRecord, DepDataConRecordEntry)
mget_tyConRec_dataConRec (ti, di) = do
    graph <- get
    let t = get_record graph ti
    return (t, tyr_dataCons t !! di)

mtyConRec_index :: TyCon.TyCon -> TyDepMonad (Maybe Int)
mtyConRec_index tc = do
    graph <- get
    return $ tyConRec_index graph tc

mdataConRec_index :: DataCon.DataCon -> TyDepMonad (Maybe (Int, Int))
mdataConRec_index dc = do
    graph <- get
    return $ dataConRec_index graph dc

-- returns the index of the inserted tyRec
madd_tyRec :: DepGraphTypeRecord -> TyDepMonad (Int)
madd_tyRec ntyr = do
    graph <- get
    let recs = dep_records graph

    let nrecs = recs ++ [ntyr]
    let ngraph = graph { dep_records = nrecs }
    put ngraph

    let i = (length nrecs) - 1
    return i

mreplace_tyRec :: Int -> DepGraphTypeRecord -> TyDepMonad ()
mreplace_tyRec ti ntyr = do
    graph <- get
    let recs = dep_records graph

    let nrecs = replaceNth ti ntyr recs
    let ngraph = graph { dep_records = nrecs }
    put ngraph

madd_dataConRec :: Int -> DepDataConRecordEntry -> TyDepMonad (Int)
madd_dataConRec ti ndce = do
    tyr <- mget_tyConRec ti
    let dcs = tyr_dataCons tyr
    
    let ndcs = dcs ++ [ndce]
    let ntyr = tyr { tyr_dataCons = ndcs }
    mreplace_tyRec ti ntyr

    let i = (length ndcs) - 1
    return i

mreplace_dataConRec :: (Int, Int) -> DepDataConRecordEntry -> TyDepMonad ()
mreplace_dataConRec is@(ti, di) ndce = do
    (tyr, dce) <- mget_tyConRec_dataConRec is
    let dcs = tyr_dataCons tyr
    let args = dce_args dce

    let ndcs = replaceNth di ndce dcs
    
    let ntyr = tyr { tyr_dataCons = ndcs }
    mreplace_tyRec ti ntyr


-- Just int if exists already, Nothing if new
madd_tyCon :: TyCon.TyCon -> TyDepMonad (Maybe Int)
madd_tyCon tc = do
    mi <- mtyConRec_index tc
    case mi of
      Nothing -> do
        let nrec = mkDepGraphTypeRecord tc
        i <- madd_tyRec nrec

        let dcs = TyCon.tyConDataCons tc
        mapM_ (madd_dataCon i) dcs
        return Nothing

      Just i -> return mi
    
-- index of tyCon for datacon
madd_dataCon :: Int -> DataCon.DataCon -> TyDepMonad ()
madd_dataCon ti dc = do
    graph <- get
    let recs = dep_records graph
    tyr <- mget_tyConRec ti

    let tc = tyr_tyCon tyr
    let dcs = tyr_dataCons tyr

    let mi = findIndex_s (==dc) (dce_dataCon) dcs
    
    case mi of
      Nothing -> do
        let ndce = mkDepDataConRecordEntry dc
        i <- madd_dataConRec ti ndce
        
        let args = DataCon.dataConOrigArgTys dc
        mapM_ (madd_dataConArg (ti, i)) args

      Just i -> return ()

mis_recursive' :: Int -> TyCon.TyCon -> [TyCon.TyCon] -> TyDepMonad (Bool)
mis_recursive' ti_target subject visited = do
    graph <- get
    ttyr <- mget_tyConRec ti_target
    let tc = tyr_tyCon ttyr

    let is_same = tc == subject
    let is_visited = elem subject visited

    if not is_same then do
      if not is_visited then do
          let sdcs = TyCon.tyConDataCons subject
          let sargs = concatMap (DataCon.dataConOrigArgTys) sdcs
          
          let isrec t = case t of
                  TyCoRep.TyConApp tc _ -> do
                    mi <- mtyConRec_index tc
                    case mi of
                      Nothing -> return False
                      Just i -> do
                        ntyr <- mget_tyConRec i
                        mis_recursive' ti_target (tyr_tyCon ntyr) (subject : visited)
                  _ -> return False

          args_rec <- mapM (isrec) sargs

          return $ any (id) args_rec
      else
        return False -- visited, not-same tycons are ignored
    else
      return True

-- check if tycon at index ti_subject directly or indirectly recuses to
-- tycon at index ti_target
mis_recursive :: Int -> TyCon.TyCon -> TyDepMonad (Bool)
mis_recursive ti_target subject = mis_recursive' ti_target subject []

madd_dataConArg' :: (Int, Int) -> [Int] -> TyCoRep.Type -> TyDepMonad ()
madd_dataConArg' is@(ti, di) arg_stack ty = do
    graph <- get
    let recs = dep_records graph

    let get_tc_from_ty ty = case ty of
          TyCoRep.TyConApp tc ts -> tc
          TyCoRep.TyVarTy var -> 
            if Var.isTyVar var then
                get_tc_from_ty $ Var.varType var
            else
                undefined
          _ -> undefined

    let tC = get_tc_from_ty ty

    let add_arg :: [Int] -> [RecTy] -> RecTy -> TyDepMonad ([RecTy], Int)
        add_arg istack tcarglist narg =
            case istack of
                [] -> return (tcarglist ++ [narg], length tcarglist)
                h:tail -> do
                    let f = tcarglist !! h
                    case f of
                      Tc t ntcarglist -> do
                        (nextntcarglist, n) <- add_arg tail ntcarglist narg
                        let r = Tc t nextntcarglist
                        let retlist = replaceNth h r tcarglist
                        return (retlist, n)

                      FunTy ntcarglist -> do
                        (nextntcarglist, n) <- add_arg tail ntcarglist narg
                        let r = FunTy nextntcarglist
                        let retlist = replaceNth h r tcarglist
                        return (retlist, n)

                      _ ->
                        error "invalid add"

    let get_recTy istack arglist =
            case istack of
                h:[] -> arglist !! h
                h:tail -> do
                    let f = arglist !! h
                    case f of
                      Tc _ ntcarglist -> get_recTy tail ntcarglist
                      FunTy ntcarglist -> get_recTy tail ntcarglist
                      _ ->
                        error "invalid index"

    let plainAdd_tyCon tc ts = do
            dce <- mget_dataConRec is
            let args = dce_args dce

            (nargs, i) <- add_arg arg_stack args (Tc tc [])

            let ndce = dce { dce_args = nargs }
            mreplace_dataConRec is ndce

            mapM_ (madd_dataConArg' is (arg_stack ++ [i])) ts
            let ret = get_recTy (arg_stack ++ [i]) nargs
            return ret

    let add_funTy t1 t2 = do
            dce <- mget_dataConRec is
            let args = dce_args dce
            let narg = FunTy []

            (nargs, i) <- add_arg arg_stack args narg

            let ndce = dce { dce_args = nargs }
            mreplace_dataConRec is ndce

            madd_dataConArg' is (arg_stack ++ [i]) t1
            madd_dataConArg' is (arg_stack ++ [i]) t2

            let ret = get_recTy (arg_stack ++ [i]) nargs
            return ret

    let recAdd i = do
            dce <- mget_dataConRec is
            let args = dce_args dce
            let narg = Rec i

            (nargs, _) <- add_arg arg_stack args narg

            let ndce = dce { dce_args = nargs }
            mreplace_dataConRec is ndce
            return narg

    let add_tyCon tc ts = do
            ri <- madd_tyCon tc
            nti <- fromJust <$> mtyConRec_index tc

            isrec <- mis_recursive ti tc
            if isrec then
                recAdd nti
            else
                plainAdd_tyCon tc ts
        
    let add_ty ty = case ty of
          TyCoRep.TyConApp tc ts -> add_tyCon tc ts

          TyCoRep.TyVarTy var -> do
            if Var.isTyVar var then do
              let t = Var.varType var
              add_ty t
            else
              undefined

          TyCoRep.FunTy _ t1 t2 -> do
            add_funTy t1 t2

          TyCoRep.LitTy tl -> do
            error "fug"
          _ -> undefined

    add_ty ty
{-
      TyCoRep.AppTy t1 t2 -> 
      TyCoRep.FunTy _ t arg ->
      TyCoRep.ForAllTy _ _ -> 
      TyCoRep.LitTy _ -> 
      TyCoRep.CastTy _ _ ->
      TyCoRep.CoercionTy _ ->
-}

    return ()

madd_dataConArg :: (Int, Int) -> TyCoRep.Type -> TyDepMonad ()
madd_dataConArg is ty = madd_dataConArg' is [] ty

-- sorts the datacons of the tycons, NOT the tycons
msort :: TyDepMonad ()
msort = do
    graph <- get
    let tyrs = dep_records graph

    ntyrs <- mapM msort_tyr tyrs

    put $ graph { dep_records = ntyrs }
    return ()

msort_tyr :: DepGraphTypeRecord -> TyDepMonad (DepGraphTypeRecord)
msort_tyr tyr = do
    let dces = tyr_dataCons tyr
    ndces <- msort_dces dces
    return $ tyr { tyr_dataCons = ndces }

msort_dces :: [DepDataConRecordEntry] -> TyDepMonad [DepDataConRecordEntry]
msort_dces = sortByM mcompare_dataConRec

mcompare_dataConRec :: DepDataConRecordEntry -> DepDataConRecordEntry -> TyDepMonad (Ordering)
mcompare_dataConRec l r = do
    let ldc = dce_dataCon l
    let rdc = dce_dataCon r

    if ldc == rdc then
      return EQ
    else do
      let largs = dce_args l
      let rargs = dce_args r
      let ltag = DataCon.dataConTag ldc
      let rtag = DataCon.dataConTag rdc
      
      let cmp1 = compare (length largs) (length rargs)
      cmp2 <- mcompare_dataConRec_args largs rargs
      let cmp3 = compare ltag rtag

      return $ order [cmp1, cmp2, cmp3]

mcompare_dataConRec_args :: [RecTy] -> [RecTy] -> TyDepMonad (Ordering)
mcompare_dataConRec_args []     []     = return EQ
mcompare_dataConRec_args []     (_:_)  = return LT
mcompare_dataConRec_args (_:_)  []     = return GT
mcompare_dataConRec_args (x:xs) (y:ys) = do
    mcomp <- mcompare_dataConRec_arg x y
    case mcomp of 
        EQ    -> mcompare_dataConRec_args xs ys
        other -> return other

mcompare_dataConRec_arg :: RecTy -> RecTy -> TyDepMonad (Ordering)
mcompare_dataConRec_arg (Tc ltc largs) (Tc rtc rargs) =
    if ltc == rtc then
      mcompare_dataConRec_args largs rargs
    else do
      li <- mtyConRec_index ltc
      ri <- mtyConRec_index rtc
      if li == ri then
        return EQ
      else do
        ltyr <- mget_tyConRec $ fromJust li
        rtyr <- mget_tyConRec $ fromJust ri
        mcompare_typeRecord ltyr rtyr

mcompare_dataConRec_arg (FunTy ll) (FunTy rl) =
    mcompare_dataConRec_args ll rl

mcompare_dataConRec_arg (Rec i) (Rec j)  = return $ compare i j

mcompare_dataConRec_arg a b =
    return $ compare (recTy_constructor_order a) (recTy_constructor_order b)

mcompare_typeRecord :: DepGraphTypeRecord -> DepGraphTypeRecord -> TyDepMonad (Ordering)
mcompare_typeRecord a b =
    if a == b then
      return EQ
    else do
      let adc = tyr_dataCons a
      let bdc = tyr_dataCons b

      let lencmp = compare (length adc) (length bdc)

      if lencmp == EQ then
        mcompare_dataConRecs (tyr_dataCons a) (tyr_dataCons b)
      else
        return lencmp

mcompare_dataConRecs :: [DepDataConRecordEntry] -> [DepDataConRecordEntry] -> TyDepMonad (Ordering)
mcompare_dataConRecs []     []     = return EQ
mcompare_dataConRecs []     (_:_)  = return LT
mcompare_dataConRecs (_:_)  []     = return GT
mcompare_dataConRecs (x:xs) (y:ys) = do
    mcomp <- mcompare_dataConRec x y
    case mcomp of 
        EQ    -> mcompare_dataConRecs xs ys
        other -> return other

dep_add_tyCon' :: TyCon.TyCon -> TyDepMonad ()
dep_add_tyCon' tc = do
    madd_tyCon tc
    msort
    return ()

dep_add_tyCon :: TyDepGraph -> TyCon.TyCon -> TyDepGraph
dep_add_tyCon graph tc = execState (dep_add_tyCon' tc) graph

dep_graph_from_tyCon :: TyCon.TyCon -> TyDepGraph
dep_graph_from_tyCon = dep_add_tyCon emptyTyDepGraph
