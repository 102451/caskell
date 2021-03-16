{-|
 - Hashing Context module to provide the Context monad
|-}

module Caskell.Context
(
    Context(..),
    UniqueHash(..),
    CoreData(..),
    coreData,
    hash_data_ref,
    name,
    uniq,
    HashedData(..),
    hashed_data_typename,
    UniqueHashMap,
    CtxMonad,
    mkCtx,
    empty_context,

    get_context,
    get_hashes,
    set_hashes,
    get_mod_guts,
    set_mod_guts,
    is_debug,
    set_debug,

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
    short_unique_hash_str,

    -- utility
    Caskell.Context.print,
    Caskell.Context.println,
    dprint,
    dprintln
) where

import qualified Unique
import qualified Name
import qualified CoreSyn
import qualified Var
import qualified TyCon
import qualified DataCon
import qualified GHC
import Control.Monad.State

import Data.List
import Data.List.Split
import Data.Map.MultiKey
import Data.Maybe
import Caskell.Bytes
import Caskell.Hash

default_print = True

data Context = Ctx
    { unique_hashes :: UniqueHashMap
    , mod_guts :: GHC.CoreModule
    , debug :: Bool
    }

instance Show Context where
    show = show . unique_hashes

mkCtx = Ctx {
unique_hashes = empty,
mod_guts = undefined,
debug = default_print
}

empty_context = mkCtx

type UniqueHashMap = MultiKey UniqueHash
type CoreHashMap = MultiKey CoreData

data UniqueHash = UniqueHash
    { hash_core_data :: CoreHashMap -- a set of uniques and expressios that all have the same hash
    , hash :: Hash

    -- the original definition
    , unique_definition_ref :: Maybe (CoreData)
    }

data CoreData = CoreData
    { hash_unique :: Unique.Unique
    , hash_data :: HashedData
    , hole :: Bool -- hole to be filled later

    -- whether the HashedData has a core definition or not.
    -- the first hashed non-Var HashedData has no reference,
    -- all other HashedData sharing the same hash point to
    -- the first HashedData.
    -- A reference is never a Var.
    -- Vars always point to the definition, unless there is none.
    , core_definition_ref :: Maybe (HashedData)
    
    -- whether there is a definition available at all.
    -- mainly used for external functions.
    -- there is no definition available for some global Var.Ids e.g. unpackCString#
    -- that have no unfolding.
    , definitionless :: Bool
    }

hash_data_ref :: UniqueHash -> Maybe HashedData
hash_data_ref h = do
    ref1 <- unique_definition_ref h
    return $ hash_data ref1

coreData uniq hdata hole = CoreData uniq hdata hole Nothing False

data HashedData = CoreBind CoreSyn.CoreBind -- direct expression
                | Var Var.Var -- typed reference to expression
                | TyCon TyCon.TyCon -- type constructor (basically: a data type)
                | DataCon DataCon.DataCon -- data constructor
    
hashed_data_typename x = case x of
    CoreBind _ -> "Cb" 
    Var _      -> "Var"
    TyCon _    -> "Tc"
    DataCon _  -> "Dc"

name :: HashedData -> Maybe Name.Name
name (CoreBind cb) = case cb of
    CoreSyn.NonRec b _ -> name (Var b)
    CoreSyn.Rec _ -> undefined -- TODO: recursive

name (Var v) = Just (Var.varName v)
name (TyCon tc) = Just (TyCon.tyConName tc)
name (DataCon dc) = Just (DataCon.dataConName dc)
--name _ = Nothing

uniq :: HashedData -> Maybe Unique.Unique
uniq hs = do
    nam <- name hs
    return $ Name.nameUnique nam

null_hash = get_hash ([]::[Int])

short_hash_data_name uh = s where
    n = name uh
    s = case n of
        Nothing -> "[no name]"
        Just nam -> short_name nam
short_name = last . splitOn "$" . Name.nameStableString

short_hash_str h = if h == null_hash then "NULL"
                   else take 10 $ show h
short_unique_hash_str = short_hash_str . hash

instance MultiKeyable UniqueHash where
    empty = MultiKey [key hash]

instance MultiKeyable CoreData where
    empty = MultiKey [key hash_unique]

instance Show UniqueHash where
    show a = show (hash_core_data a) ++ ref ++ ": " ++ short_unique_hash_str a where
        ref = case hash_data_ref a of
                Just b -> " -> " ++ short_hash_data_name b ++ "<" ++ hashed_data_typename b ++ ">"
                Nothing -> ""

instance Show CoreData where
    show a = short_hash_data_name (hash_data a) ++ "<" ++ typ ++ flags ++ ">" ++ ref where
        typ = hashed_data_typename $ hash_data a

        holef = case hole a of
                    True -> "h"
                    False -> ""
        deflessf = case definitionless a of
                    True -> "d"
                    False -> ""
        flags' = intercalate "," $ filter (/="") [holef, deflessf]
        flags = if Data.List.null flags' then "" else "(" ++ flags' ++ ")"

        ref = case core_definition_ref a of
                Just b -> " -> " ++ short_hash_data_name b ++ "<" ++ hashed_data_typename b ++ ">"
                Nothing -> ""


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
set_hashes hs = state $ \ctx -> ((), ctx { unique_hashes = hs })

get_mod_guts :: CtxMonad GHC.CoreModule
get_mod_guts = state $ \ctx -> (mod_guts ctx, ctx)

set_mod_guts :: GHC.CoreModule -> CtxMonad ()
set_mod_guts guts = state $ \ctx -> ((), ctx { mod_guts = guts })

is_debug :: CtxMonad Bool
is_debug = state $ \ctx -> (debug ctx, ctx)

set_debug :: Bool -> CtxMonad ()
set_debug dbg = state $ \ctx -> ((), ctx { debug = dbg })

-- other methods
lookup_name :: String -> Context -> Maybe UniqueHash
lookup_name key ctx = result' where
    hashes = unique_hashes ctx
    hashes' = Data.Map.MultiKey.toList hashes
    cd = map (\x -> (Data.Map.MultiKey.toList $ hash_core_data x, x)) hashes'
    lookups = map (\(x, y) -> (any ((==key) . short_hash_data_name . hash_data) x, y)) cd
    result = find (fst) lookups

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

    uq <- case prev of
            Nothing -> do
              let chm = Data.Map.MultiKey.insert cd empty
              let ref = Just cd -- vars can never be first
              return $ UniqueHash chm h ref

            Just uh -> do
              let hcd = hash_core_data uh
              let cd' = cd { core_definition_ref = hash_data_ref uh }
              let chm = Data.Map.MultiKey.insert cd' hcd

              return $ uh { hash_core_data = chm }
        
    let newhashes = Data.Map.MultiKey.insert uq hashes
        
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

-- utility
print :: String -> CtxMonad ()
print = lift . putStr

println :: String -> CtxMonad ()
println = lift . putStrLn

dprint :: String -> CtxMonad ()
dprint s = do
    dbg <- is_debug
    when dbg (Caskell.Context.print s)

dprintln :: String -> CtxMonad ()
dprintln s = do
    dbg <- is_debug
    when dbg (Caskell.Context.println s)
