{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Trie.Internal where

import Data.Foldable (Foldable)
import Data.Map (Map)
import Data.Traversable (Traversable)


data Trie k v = Trie
      { value    :: v
      , end      :: Bool
      , children :: Map k (Trie k v)
      } deriving (Foldable, Functor, Read, Eq, Show, Traversable)
