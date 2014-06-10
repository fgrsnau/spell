{-# LANGUAGE BangPatterns #-}
module Data.Trie
    ( Trie
    , empty
    , insert, delete, delete', cut, cut', update, prune
    , lookup, value, end, branches
    , fromList, toList, skeleton, populate
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


empty :: v -> Trie k v
empty v = Trie v False M.empty

prune :: Ord k => Trie k v -> Trie k v
prune t' = fromMaybe (t' { children = M.empty }) (go t')
  where
    go t = do
      let newChildren = M.mapMaybe go (children t)
      guard (end t || not (M.null newChildren))
      return $ t { children = newChildren }

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

delete :: (ListLike l k, Ord k) => l -> Trie k v -> Trie k v
delete ks = prune . delete' ks

delete' :: (ListLike l k, Ord k) => l -> Trie k v -> Trie k v
delete' ks' t
  | LL.null ks' = t { end = False }
  | otherwise  = t { children = M.adjust (delete' ks) k (children t) }
  where
    (k, ks) = (LL.head ks', LL.tail ks')

update :: Ord k => (w -> [k] -> [v] -> v) -> Trie k w -> Trie k v
update f = go [] []
  where
    go !ks !vs t = t { value    = v
                     , children = M.mapWithKey call (children t)
                     }
      where
        v      = f (value t) ks vs
        call k = go (k : ks) (v : vs)


cut :: Ord k => (v -> Bool) -> Trie k v -> Trie k v
cut f = prune . cut' f

cut' :: Ord k => (v -> Bool) -> Trie k v -> Trie k v
cut' p t
  | p (value t) = t { children = M.empty }
  | otherwise   = t { children = M.map (cut' p) (children t) }

lookup :: (ListLike l k, Ord k) => l -> Trie k v -> Maybe v
lookup ks t
  | LL.null ks = guard (end t) >> Just (value t)
  | otherwise  = M.lookup (LL.head ks) (children t) >>= lookup (LL.tail ks)

branches :: Trie k v -> [(k, Trie k v)]
branches t = M.toList (children t)

toList :: (ListLike l k, Ord k) => Trie k v -> [(l, v)]
toList t
  | end t     = (LL.empty, value t) : rest
  | otherwise = rest
  where
    rest = do
      (k, c) <- M.toList (children t)
      map (first (LL.cons k)) (toList  c)

fromList :: (ListLike lk k, Ord k)
            => ([k] -> [v] -> v) -> [lk] -> Trie k v
fromList f = foldr (insert f) (empty (f [] []))

skeleton :: (ListLike lk k, Ord k) => [lk] -> Trie k ()
skeleton = fromList (\_ _ -> ())

populate :: Ord k => ([k] -> [v] -> v) -> Trie k () -> Trie k v
populate f = update (const f)

expandPaths :: (ListLike lk k, Ord k) => Trie k v -> Trie k (lk, v)
expandPaths = update (\v ps _ -> (LL.reverse (LL.fromList ps), v))
