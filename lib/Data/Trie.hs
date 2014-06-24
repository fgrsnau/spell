{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Safe         #-}

-- |
-- Module      : Data.Trie
-- Description : Implemention of prefix trees.
-- Copyright   : (c) Stefan Haller, 2014
--
-- License     : MIT
-- Maintainer  : s6171690@mail.zih.tu-dresden.de
-- Stability   : experimental
-- Portability : portable
--
-- A 'Trie' implementation. See <http://en.wikipedia.org/wiki/Trie>
-- for more details.
--
-- This module contains the public API.
--
-- Many of these functions use 'ListLike' instances as paths and so it
-- is possible to use more efficient data types than lists.
module Data.Trie
    ( -- * Trie data type
      Trie

      -- * Operations for single nodes
    , empty
    , value, end, branches

      -- * Operations acting recursively
    , insert, delete, delete', cut, cut', update, prune
    , lookup

      -- * Bulk operations
    , fromList, toList, skeleton, populate

      -- * Utility functions
    , expandPaths
    ) where

import           Prelude hiding (lookup)

import           Control.Arrow (first)
import           Control.Monad (guard)

import           Data.ListLike.Base (ListLike)
import qualified Data.ListLike.Base as LL
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Trie.Internal (Trie(..))


-- | Creates an empty root node with value @v@.
empty :: v -> Trie k v
empty v = Trie v False M.empty

-- | Removes all branches in the 'Trie' which never lead to an end node.
prune :: Ord k => Trie k v -> Trie k v
prune t' = fromMaybe (t' { children = M.empty }) (go t')
  where
    go t = do
      let newChildren = M.mapMaybe go (children t)
      guard (end t || not (M.null newChildren))
      return $ t { children = newChildren }

-- | Inserts the path into the 'Trie'. The value for all newly created
-- nodes is determined by the return value of the supplied function. The
-- function gets the whole path to the node and all previous values on the
-- current path as arguments.
--
-- The values of existing nodes is not changed. If the given path was
-- already inserted into the current 'Trie', this operation is a no-op.
insert :: (ListLike lk k, Ord k)
          => ([k] -> [v] -> v) -> lk -> Trie k v -> Trie k v
insert f = go [] []
  where
    go !ps !vs ks t
      | LL.null ks = t { end = True }
      | otherwise  = t { children = M.alter func (LL.head ks) (children t) }
      where
        ps' = LL.head ks : ps
        vs' = value t : vs
        call = go ps' vs' (LL.tail ks)
        func Nothing  = Just (call (Trie (f ps' vs') False M.empty))
        func (Just c) = Just (call c)

-- | Like 'delete'' with additional pruning, see 'prune'.
delete :: (ListLike l k, Ord k) => l -> Trie k v -> Trie k v
delete ks = prune . delete' ks

-- | Deletes a node described by the path from the 'Trie', i.e. disables
-- the end flag of the node.
delete' :: (ListLike l k, Ord k) => l -> Trie k v -> Trie k v
delete' ks' t
  | LL.null ks' = t { end = False }
  | otherwise  = t { children = M.adjust (delete' ks) k (children t) }
  where
    (k, ks) = (LL.head ks', LL.tail ks')

-- | Updates the values of all nodes with the help of the supplied
-- function. This works the same like 'insert', but the function is
-- called with an additional first argument, the old value of the
-- current node.
update :: Ord k => (w -> [k] -> [v] -> v) -> Trie k w -> Trie k v
update f = go [] []
  where
    go !ks !vs t = t { value    = v
                     , children = M.mapWithKey call (children t)
                     }
      where
        v      = f (value t) ks vs
        call k = go (k : ks) (v : vs)

-- | Like 'cut'' with additional pruning, see 'prune'.
cut :: Ord k => (v -> Bool) -> Trie k v -> Trie k v
cut f = prune . cut' f

-- | Removes the children of all nodes for which the predicate holds.
cut' :: Ord k => (v -> Bool) -> Trie k v -> Trie k v
cut' p t
  | p (value t) = t { children = M.empty }
  | otherwise   = t { children = M.map (cut' p) (children t) }

-- | Returns the value of the given path. If the path does not lead to
-- an end node, Nothing is returned.
lookup :: (ListLike l k, Ord k) => l -> Trie k v -> Maybe v
lookup ks t
  | LL.null ks = guard (end t) >> Just (value t)
  | otherwise  = M.lookup (LL.head ks) (children t) >>= lookup (LL.tail ks)

-- | Returns all branches of the current node.
branches :: Trie k v -> [(k, Trie k v)]
branches t = M.toList (children t)

-- | Returns a list of all end nodes and their respective paths.
toList :: (ListLike l k, Ord k) => Trie k v -> [(l, v)]
toList t
  | end t     = (LL.empty, value t) : rest
  | otherwise = rest
  where
    rest = do
      (k, c) <- M.toList (children t)
      map (first (LL.cons k)) (toList  c)

-- | Builds a 'Trie' from a list of paths and calculates the value for
-- all nodes.
--
-- All paths get inserted into the 'Trie'. The value of the nodes is
-- determined by the supplied function. See 'insert' for more details.
fromList :: (ListLike lk k, Ord k)
            => ([k] -> [v] -> v) -> [lk] -> Trie k v
fromList f = foldr (insert f) (empty (f [] []))

-- | Builds a 'Trie' from a list of paths without attaching any
-- values.
--
-- The same as 'fromList' but all node values are set to an empty
-- tuple.
skeleton :: (ListLike lk k, Ord k) => [lk] -> Trie k ()
skeleton = fromList (\_ _ -> ())

-- | Populates values of a skeleton.
--
-- This is a simplified 'update' function for 'Trie's createy by the
-- 'skeleton' function.
populate :: Ord k => ([k] -> [v] -> v) -> Trie k () -> Trie k v
populate f = update (const f)

-- | Prepends the path of the nodes to the their respective values.
expandPaths :: (ListLike lk k, Ord k) => Trie k v -> Trie k (lk, v)
expandPaths = update (\v ps _ -> (LL.reverse (LL.fromList ps), v))
