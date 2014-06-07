{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Trie.Internal where

import Data.Binary (Binary)
import Data.Foldable (Foldable)
import Data.Map (Map)
import Data.Traversable (Traversable)

import GHC.Generics (Generic)


data Trie k v = Trie
      { value    :: v
      , end      :: Bool
      , children :: Map k (Trie k v)
      } deriving (Foldable, Functor, Generic, Read, Eq, Show, Traversable)


instance (Binary k, Binary v) => Binary (Trie k v) where
