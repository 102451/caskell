{-|
 - Hashing Context module to provide the Context monad
|-}

module Caskell.Context
(
    Context(..),
    UniqueHash(..),
    HashedData(..),
    UniqueHashMap,
    CtxMonad,
    mkCtx,
    empty_context,

    get_hashes,
    set_hashes,
    get_mod_guts,
    set_mod_guts,

    get,
    put,
    show,

    Data.Map.MultiKey.lookup,
    lookup_name, -- only for debug
    lookup_unique,
    lookup_hash,
    insert_hash,

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

import Data.List.Split
import Data.Map.MultiKey
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

data UniqueHash = UniqueHash
    { hash_unique :: Unique.Unique
    , hash_data :: HashedData
    , hash :: Hash
    , hole :: Bool -- hole to be filled later
    }

data HashedData = CoreBind CoreSyn.CoreBind
                | Var Var.Var
    
name :: HashedData -> Maybe Name.Name
name (CoreBind cb) = case cb of
    CoreSyn.NonRec b _ -> name (Var b)
    CoreSyn.Rec _ -> undefined -- TODO: recursive

name (Var v) = Just (Var.varName v)
name _ = Nothing

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
    empty = MultiKey [key (short_hash_data_name . hash_data), key hash_unique, key hash]

instance Show UniqueHash where
    show a = short_hash_data_name (hash_data a) ++ ": " ++ short_unique_hash_str a ++ t where
        t = case hole a of
            True -> "(h)"
            False -> ""
           

instance Show a => Show (MultiKey a) where
    show = show . toList

instance Ord Unique.Unique where
    a <= b = Unique.getKey a <= Unique.getKey b


type CtxMonad a = StateT Context IO a

get_hashes :: CtxMonad UniqueHashMap
get_hashes = state $ \ctx -> (unique_hashes ctx, ctx)

set_hashes :: UniqueHashMap -> CtxMonad ()
set_hashes hs = state $ \ctx -> ((), ctx { unique_hashes = hs})

get_mod_guts :: CtxMonad GHC.CoreModule
get_mod_guts = state $ \ctx -> (mod_guts ctx, ctx)

set_mod_guts :: GHC.CoreModule -> CtxMonad ()
set_mod_guts guts = state $ \ctx -> ((), ctx { mod_guts = guts})

lookup_name :: String -> CtxMonad (Maybe UniqueHash)
lookup_name key = do
    hashes <- get_hashes
    return $ Data.Map.MultiKey.lookup key hashes

lookup_unique :: Unique.Unique -> CtxMonad (Maybe UniqueHash)
lookup_unique key = do
    hashes <- get_hashes
    return $ Data.Map.MultiKey.lookup key hashes

lookup_hash :: Hash -> CtxMonad (Maybe UniqueHash)
lookup_hash key = do
    hashes <- get_hashes
    return $ Data.Map.MultiKey.lookup key hashes

insert_hash :: UniqueHash -> CtxMonad ()
insert_hash h = do
    hashes <- get_hashes
    let newhashes = insert h hashes
    set_hashes newhashes
