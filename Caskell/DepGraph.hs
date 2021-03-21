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

import Control.Monad.State
import Data.List
import Data.List.Split
import Data.Maybe

import Caskell.CoreCompare

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

data RecTy = Ty TyCon.TyCon [RecTy] -- arguments
           | Rec Int -- int = index in records, ONLY for recursive tycons

type TyDepGraph = DepGraph DepGraphTypeRecord

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
                Ty ty args -> s' where
                    ts = show ty 
                    argss = intercalate " " $ map (rectys) args
                    s' = if null argss then ts else ts ++ " (" ++ argss ++ ")"

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

-- selector function
findIndex_s :: Eq a => (a -> Bool) -> (b -> a) -> [b] -> Maybe Int
findIndex_s p s l = loop 0 l
                 where
                   loop _ [] = Nothing
                   loop n (x:xs) | p (s x)   = Just n
                                 | otherwise = loop (n + 1) xs

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
short_name = last . splitOn "$" . Name.nameStableString
-- https://stackoverflow.com/a/5852820
replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs

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

    let add_arg istack tyarglist narg =
            case istack of
                [] -> return $ tyarglist ++ [narg]
                h:tail -> do
                    let (Ty t ntyarglist) = tyarglist !! h
                    nextntyarglist <- add_arg tail ntyarglist narg
                    let r = Ty t nextntyarglist
                    let retlist = replaceNth h r tyarglist
                    return retlist

    let plainAdd ts = do
            dce <- mget_dataConRec is
            let args = dce_args dce

            nargs <- add_arg arg_stack args (Ty ty [])

            let ndce = dce { dce_args = nargs }
            mreplace_dataConRec is ndce

            let i = (length nargs) - 1
            mapM_ (madd_dataConArg' is (arg_stack ++ [i])) ts

    let recAdd i = do
            dce <- mget_dataConRec is
            let args = dce_args dce

            nargs <- add_arg arg_stack args (Rec i)

            let ndce = dce { dce_args = nargs }
            mreplace_dataConRec is ndce

    let add_tyCon tc ts = do
            ri <- madd_tyCon tc
            nti <- fromJust <$> mtyConRec_index tc

            case ri of
                Nothing -> do
                    isrec <- mis_recursive ti tc
                    if isrec then
                        recAdd nti
                    else
                        plainAdd ts

                Just i -> recAdd i
        
    let add_ty ty = case ty of
          TyCoRep.TyConApp tc ts -> add_tyCon tc ts

          TyCoRep.TyVarTy var -> do
            if Var.isTyVar var then do
              let t = Var.varType var
              add_ty t
            else
              undefined
          _ -> undefined

    add_ty ty
{-
      TyCoRep.AppTy t1 t2 -> concat ["(", show t1, " ", show t2, ")"]

      TyCoRep.FunTy _ t arg -> concat [show t, " -> ", show arg]
      TyCoRep.ForAllTy _ _ -> 
      TyCoRep.LitTy _ -> 
      TyCoRep.CastTy _ _ -> ""
      TyCoRep.CoercionTy _ -> ""
-}

    return ()
    

madd_dataConArg :: (Int, Int) -> TyCoRep.Type -> TyDepMonad ()
madd_dataConArg is ty = madd_dataConArg' is [] ty

dep_graph_from_tyCon' :: TyCon.TyCon -> TyDepMonad ()
dep_graph_from_tyCon' tc = do
    let tyrec = DepGraphTypeRecord { tyr_tyCon = tc, tyr_dataCons = [] }
    let dep = DepGraph { dep_records = [] }
    put dep
    madd_tyCon tc
    -- TODO: stable sort the datacons
    return ()

dep_graph_from_tyCon :: TyCon.TyCon -> TyDepGraph
dep_graph_from_tyCon tc = execState (dep_graph_from_tyCon' tc) emptyTyDepGraph
