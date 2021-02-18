{-|
 - Hashing Context module to provide the Context monad
|-}

module Caskell.Context
(
    Context(..),
    StateCtx,
    mkCtx,
    empty_context,

    get_bind_hashes,
    set_bind_hashes,

    get,
    put,
    runState,
    evalState,
    execState
) where

import Control.Monad.State

import Caskell.Bytes
import Caskell.Hash


data Context = Ctx
    { bind_hashes :: [(String, Hash)]
    } deriving (Show)

type StateCtx a = State Context a
type StateCtx' = StateCtx Int

mkCtx = Ctx {
bind_hashes = []
}

empty_context = mkCtx

get_bind_hashes :: StateCtx [(String, Hash)]
get_bind_hashes = state $ \ctx -> (bind_hashes ctx, ctx)

set_bind_hashes :: [(String, Hash)] -> StateCtx ()
set_bind_hashes hs = state $ \ctx -> ((), ctx { bind_hashes = hs})

