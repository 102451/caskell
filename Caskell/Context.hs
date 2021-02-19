{-|
 - Hashing Context module to provide the Context monad
|-}

module Caskell.Context
(
    Context(..),
    BindHash(..),
    BindHashMap,
    CtxMonad,
    mkCtx,
    empty_context,

    get_bind_hashes,
    set_bind_hashes,

    get,
    put,
    show,

    lookup_name, -- only for debug
    lookup_unique,
    lookup_hash,
    insert_hash,

    short_name,
    short_hash_str
) where

import qualified Unique
import qualified Name
import qualified CoreSyn
import Control.Monad.State

import Data.List.Split
import Data.Map.MultiKey
import Caskell.Bytes
import Caskell.Hash

data Context = Ctx
    { bind_hashes :: BindHashMap
    } deriving (Show)

mkCtx = Ctx {
bind_hashes = empty
}

type BindHashMap = MultiKey BindHash

data BindHash = BindHash
    { name :: Name.Name -- contains the unique
    , bind :: CoreSyn.CoreBind  -- just a reference
    , hash :: Hash
    }

short_name = last . splitOn "$" . Name.nameStableString . name
short_hash_str = take 10 . show . hash

instance MultiKeyable BindHash where
    empty = MultiKey [key short_name, key (Name.nameUnique . name), key hash]

instance Show BindHash where
    show a = short_name a ++ ": " ++ short_hash_str a

instance Show a => Show (MultiKey a) where
    show = show . toList

instance Ord Unique.Unique where
    a <= b = Unique.getKey a <= Unique.getKey b

type CtxMonad a = StateT Context IO a

empty_context = mkCtx

get_bind_hashes :: CtxMonad BindHashMap
get_bind_hashes = state $ \ctx -> (bind_hashes ctx, ctx)

set_bind_hashes :: BindHashMap -> CtxMonad ()
set_bind_hashes hs = state $ \ctx -> ((), ctx { bind_hashes = hs})

lookup_name :: String -> CtxMonad (Maybe BindHash)
lookup_name key = do
    binds <- get_bind_hashes
    return $ Data.Map.MultiKey.lookup key binds

lookup_unique :: Unique.Unique -> CtxMonad (Maybe BindHash)
lookup_unique key = do
    binds <- get_bind_hashes
    return $ Data.Map.MultiKey.lookup key binds

lookup_hash :: Hash -> CtxMonad (Maybe BindHash)
lookup_hash key = do
    binds <- get_bind_hashes
    return $ Data.Map.MultiKey.lookup key binds

insert_hash :: BindHash -> CtxMonad ()
insert_hash h = do
    binds <- get_bind_hashes
    let newbinds = insert h binds
    set_bind_hashes newbinds
