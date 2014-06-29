module Data.Trie
    ( Trie, TrieFunction
    , empty
    , insert, delete, delete', cut, cut', prune
    , lookup, update
    , fromList, toList, skeleton, populate
    ) where

import           Prelude hiding (lookup)

import           Control.Arrow (first)
import           Control.Monad (guard)

import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Trie.Internal (Trie(..))


type TrieFunction k v = [k] -> [v] -> v


empty :: v -> Trie k v
empty v = Trie v False M.empty

prune :: Ord k => Trie k v -> Trie k v
prune t' = fromMaybe (t' { children = M.empty }) (go t')
  where
    go t = do
      let newChildren = M.mapMaybe go (children t)
      guard (end t || not (M.null newChildren))
      return $ t { children = newChildren }

insert :: Ord k => TrieFunction k v -> [k] -> Trie k v -> Trie k v
insert f = go [] []
  where
    go _  _  []     t = t { end = True }
    go ps vs (k:ks) t = t { children = M.alter func k (children t) }
      where
        ps' = k : ps
        vs' = value t : vs
        call = go ps' vs' ks
        func Nothing  = Just (call (Trie (f ps' vs') False M.empty))
        func (Just c) = Just (call c)

delete :: Ord k => [k] -> Trie k v -> Trie k v
delete ks = prune . delete' ks

delete' :: Ord k => [k] -> Trie k v -> Trie k v
delete' []     t = t { end = False }
delete' (k:ks) t = t { children = M.adjust (delete' ks) k (children t) }

update :: Ord k => (w -> TrieFunction k v) -> Trie k w -> Trie k v
update f = go [] []
  where
    go ks vs t = t { value    = v
                   , children = M.mapWithKey call (children t)
                   }
      where
        v      = f (value t) ks vs
        call k = go (k:ks) (v:vs)


cut :: Ord k => (v -> Bool) -> Trie k v -> Trie k v
cut f = prune . cut' f

cut' :: Ord k => (v -> Bool) -> Trie k v -> Trie k v
cut' p t
  | p (value t) = t { children = M.empty }
  | otherwise   = t { children = M.map (cut' p) (children t) }

lookup :: Ord k => [k] -> Trie k v -> Maybe v
lookup []     t = guard (end t) >> Just (value t)
lookup (k:ks) t = M.lookup k (children t) >>= lookup ks

toList :: Ord k => Trie k v -> [([k], v)]
toList t
  | end t     = ([], value t) : rest
  | otherwise = rest
  where
    rest = do
      (k, c) <- M.toList (children t)
      map (first (k:)) (toList  c)

fromList :: Ord k => TrieFunction k v -> [[k]] -> Trie k v
fromList f = foldr (insert f) (empty (f [] []))

skeleton :: Ord k => [[k]] -> Trie k ()
skeleton = fromList (\_ _ -> ())

populate :: Ord k => TrieFunction k v -> Trie k () -> Trie k v
populate f = update (const f)
