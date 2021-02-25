{-|
 - Hashing Context module to provide the Context monad
|-}

module Caskell.Context
(
    Context(..),
    UniqueHash(..),
    CoreData(..),
    HashedData(..),
    UniqueHashMap,
    CtxMonad,
    mkCtx,
    empty_context,

    get_context,
    get_hashes,
    set_hashes,
    get_mod_guts,
    set_mod_guts,

    get,
    put,
    show,

    Data.Map.MultiKey.lookup,
    lookup_name, -- only for debug
    mlookup_name,
    lookup_unique,
    mlookup_unique,
    lookup_hash,
    mlookup_hash,

    mlookup_unique_in_module,

    minsert_hash,
    madd_hash,
    mdelete_by_hash,
    mdelete_by_unique,


    short_name,
    short_hash_data_name,
    short_hash_str,
    short_unique_hash_str
) where

import qualified Unique
import qualified Name
import qualified CoreSyn
import qualified Var
import qualified GHC
import Control.Monad.State

import Data.List
import Data.List.Split
import Data.Map.MultiKey
import Data.Maybe
import Caskell.Bytes
import Caskell.Hash

data Context = Ctx
    { unique_hashes :: UniqueHashMap
    , mod_guts :: GHC.CoreModule
    }

instance Show Context where
    show = show . unique_hashes

mkCtx = Ctx {
unique_hashes = empty,
mod_guts = undefined
}

empty_context = mkCtx

type UniqueHashMap = MultiKey UniqueHash
type CoreHashMap = MultiKey CoreData

data UniqueHash = UniqueHash
    { hash_core_data :: CoreHashMap -- a set of uniques and expressios that all have the same hash
    , hash :: Hash
    }

data CoreData = CoreData
    { hash_unique :: Unique.Unique
    , hash_data :: HashedData
    , hole :: Bool -- hole to be filled later
    }

data HashedData = CoreBind CoreSyn.CoreBind -- direct expression
                | Var Var.Var -- typed reference to expression
    
name :: HashedData -> Maybe Name.Name
name (CoreBind cb) = case cb of
    CoreSyn.NonRec b _ -> name (Var b)
    CoreSyn.Rec _ -> undefined -- TODO: recursive

name (Var v) = Just (Var.varName v)
--name _ = Nothing

null_hash = get_hash ([]::[Int])

short_hash_data_name uh = s where
    n = name uh
    s = case n of
        Nothing -> "[no name]"
        Just nam -> short_name nam
short_name = last . splitOn "$" . Name.nameStableString

short_hash_str = take 10 . show
short_unique_hash_str x = s where
    h = hash x
    s = if h == null_hash then "NULL"
        else short_hash_str h

instance MultiKeyable UniqueHash where
    empty = MultiKey [key hash]

instance MultiKeyable CoreData where
    empty = MultiKey [key (short_hash_data_name . hash_data), key hash_unique]

instance Show UniqueHash where
    show a = show (hash_core_data a) ++ ": " ++ short_unique_hash_str a

instance Show CoreData where
    show a = short_hash_data_name (hash_data a) ++ t where
        t = case hole a of
            True -> "(h)"
            False -> ""


instance Show a => Show (MultiKey a) where
    show = show . toList

instance Ord Unique.Unique where
    a <= b = Unique.getKey a <= Unique.getKey b

type CtxMonad a = StateT Context IO a

get_context :: CtxMonad Context
get_context = state $ \ctx -> (ctx, ctx)

get_hashes :: CtxMonad UniqueHashMap
get_hashes = state $ \ctx -> (unique_hashes ctx, ctx)

set_hashes :: UniqueHashMap -> CtxMonad ()
set_hashes hs = state $ \ctx -> ((), ctx { unique_hashes = hs})

get_mod_guts :: CtxMonad GHC.CoreModule
get_mod_guts = state $ \ctx -> (mod_guts ctx, ctx)

set_mod_guts :: GHC.CoreModule -> CtxMonad ()
set_mod_guts guts = state $ \ctx -> ((), ctx { mod_guts = guts})

-- other methods
lookup_name :: String -> Context -> Maybe UniqueHash
lookup_name key ctx = result' where
    hashes = unique_hashes ctx
    hashes' = Data.Map.MultiKey.toList hashes
    lookups = map (\x -> (Data.Map.MultiKey.lookup key $ hash_core_data x, x)) hashes'
    result = find (isJust . fst) lookups

    result' = case result of
        Nothing -> Nothing
        Just x -> Just $ snd x

mlookup_name :: String -> CtxMonad (Maybe UniqueHash)
mlookup_name key = do
    ctx <- get_context
    return $ lookup_name key ctx

lookup_unique :: Unique.Unique -> Context -> Maybe UniqueHash
lookup_unique key ctx = result' where
    hashes = unique_hashes ctx
    hashes' = Data.Map.MultiKey.toList hashes
    lookups = map (\x -> (Data.Map.MultiKey.lookup key $ hash_core_data x, x)) hashes'
    result = find (isJust . fst) lookups

    result' = case result of
        Nothing -> Nothing
        Just x -> Just $ snd x

mlookup_unique :: Unique.Unique -> CtxMonad (Maybe UniqueHash)
mlookup_unique key = do
    ctx <- get_context
    return $ lookup_unique key ctx

lookup_hash :: Hash -> Context -> Maybe UniqueHash
lookup_hash key ctx = result where
    hashes = unique_hashes ctx
    result = Data.Map.MultiKey.lookup key hashes

mlookup_hash :: Hash -> CtxMonad (Maybe UniqueHash)
mlookup_hash key = do
    hashes <- get_hashes
    return $ Data.Map.MultiKey.lookup key hashes

mlookup_unique_in_module :: Unique.Unique -> CtxMonad (Maybe CoreSyn.CoreBind)
mlookup_unique_in_module key = do
    md <- get_mod_guts
    let keymatch x = Unique.getKey key == (Unique.getKey $ Var.varUnique x)
    let varsof x = case x of
                    CoreSyn.NonRec b _ -> [b]
                    CoreSyn.Rec l -> map (fst) l

    let coreprog = GHC.cm_binds md

    let match = find (any (keymatch) . varsof) coreprog
    
    return match

minsert_hash :: UniqueHash -> CtxMonad ()
minsert_hash h = do
    hashes <- get_hashes
    let newhashes = Data.Map.MultiKey.insert h hashes
    set_hashes newhashes

madd_hash :: CoreData -> Hash -> CtxMonad ()
madd_hash cd h = do
    hashes <- get_hashes
    prev <- mlookup_hash h

    let newhashes = case prev of
                    Nothing -> nm where
                      chm = Data.Map.MultiKey.insert cd empty
                      uq = UniqueHash chm h
                      nm = Data.Map.MultiKey.insert uq hashes

                    Just uh -> nm where
                      hcd = hash_core_data uh
                      chm = Data.Map.MultiKey.insert cd hcd
                      uq = uh { hash_core_data = chm }
                      nm = Data.Map.MultiKey.insert uq hashes
        
    set_hashes newhashes

mdelete_by_hash :: Hash -> CtxMonad ()
mdelete_by_hash h = do
    hashes <- get_hashes
    let newhashes = Data.Map.MultiKey.deleteKey h hashes
    set_hashes newhashes

mdelete_by_unique :: Unique.Unique -> CtxMonad ()
mdelete_by_unique u = do
    hashes <- get_hashes
    prev <- mlookup_unique u

    let newhashes = case prev of
                    Nothing -> hashes -- do nothing

                    Just uh -> nm where
                      h = hash uh
                      hcd = hash_core_data uh
                      len = length $ Data.Map.MultiKey.toList $ hcd
                      
                      chm = Data.Map.MultiKey.deleteKey u hcd
                      uq = uh { hash_core_data = chm }

                      nm = if len < 2 then
                            Data.Map.MultiKey.deleteKey h hashes
                           else
                            Data.Map.MultiKey.insert uq hashes
        
    set_hashes newhashes
