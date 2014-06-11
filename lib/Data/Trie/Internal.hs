{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE Safe              #-}

-- |
-- Module      : Data.Trie.Internal
-- Description : Internal type representation for prefix trees.
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
-- This module contains the internal definition of the data type.
module Data.Trie.Internal where

import Data.Binary (Binary)
import Data.Foldable (Foldable)
import Data.Map (Map)
import Data.Traversable (Traversable)

import GHC.Generics (Generic)


-- | This represents a 'Trie' data structure. For more information
-- consult <http://en.wikipedia.org/wiki/Trie>.
--
-- * Each node shares a common prefix with all its children.
--
-- * Each node is tagged with a value of type @v@.
--
-- * Each node has branches to its children which are labelled with
--   values of type @k@.
--
-- * The only valid paths are paths from the root node to specially
--   labelled “end” nodes.
data Trie k v = Trie
      { value    :: v                -- ^ The value attached to this node.
      , end      :: Bool             -- ^ True if the node is considered as end node.
      , children :: Map k (Trie k v) -- ^ Branches to child nodes.
      } deriving (Foldable, Functor, Generic, Read, Eq, Show, Traversable)


-- | The default instance for Binary is using GHC's generics feature.
instance (Binary k, Binary v) => Binary (Trie k v) where
