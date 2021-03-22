{-
 - utility functions
 -}

module Caskell.Utility
(
    order,
    short_name,
    replaceNth,
    findIndex_s,
    mtrace,
    showPpr',
    sortByM
) where

import qualified Name
import qualified Debug.Trace
import Outputable (Outputable, ppr, showSDocUnsafe)

import Data.List
import Data.List.Split

-- gets the first non-EQ ordering, or EQ if its all EQ or null
order :: [Ordering] -> Ordering
order l = o where
    nl = dropWhile (==EQ) l
    o = if null nl then EQ else head nl

-- "nice" name
short_name :: Name.Name -> String
short_name = last . splitOn "$" . Name.nameStableString

-- https://stackoverflow.com/a/5852820
replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs

-- selector function
findIndex_s :: Eq a => (a -> Bool) -> (b -> a) -> [b] -> Maybe Int
findIndex_s p s l = loop 0 l
                 where
                   loop _ [] = Nothing
                   loop n (x:xs) | p (s x)   = Just n
                                 | otherwise = loop (n + 1) xs

-- monad trace
mtrace :: Monad m => String -> m ()
mtrace = flip (Debug.Trace.trace) (return ())

showPpr' :: Outputable a => a -> String
showPpr' = showSDocUnsafe . ppr

-- monad sorting
-- https://stackoverflow.com/a/11478892
sortByM :: (Monad m, Functor m) => (a -> a -> m Ordering) -> [a] -> m [a]
sortByM cmp []  = return []
sortByM cmp [x] = return [x]
sortByM cmp xs = do
  let (ys, zs) = partition xs
  ys' <- sortByM cmp ys
  zs' <- sortByM cmp zs
  merge ys' zs'
  where merge [] bs = return bs
        merge as [] = return as
        merge (a:as) (b:bs) = do
          comparison <- cmp a b
          case comparison of
            LT -> (a:) <$> merge as (b:bs)
            _  -> (b:) <$> merge (a:as) bs
        partition xs = splitAt (length xs `quot` 2) xs
