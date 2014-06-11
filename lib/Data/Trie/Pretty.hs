{-# LANGUAGE Safe #-}

-- |
-- Module      : Data.Trie.Pretty
-- Description : Pretty printing of prefix trees.
-- Copyright   : (c) Stefan Haller, 2014
--
-- License     : MIT
-- Maintainer  : s6171690@mail.zih.tu-dresden.de
-- Stability   : experimental
-- Portability : portable
--
-- The functions in this module are able to turn a 'Trie' into a
-- graphviz representation.
module Data.Trie.Pretty where

import           Prelude hiding (lookup)

import           Control.Monad.Trans.State (State, evalState, get, modify')
import qualified Data.Map as M
import           Data.Trie.Internal (Trie(..))


-- | A newtype wrapper for a 'Char' value. The 'Show' instance is
-- modified so that the character is returned without quotes.
--
-- (TODO: Do not abuse 'Show' and use proper pretty printing type
-- class.)
newtype PrettyChar = PrettyChar Char deriving (Eq, Ord, Read)

instance Show PrettyChar where
  show (PrettyChar c) = [c]


-- | Returns a 'String' containing a graphviz representation of the graph.
--
-- Save the output to a file and pass it to @dot@:
--
-- @dot -Tpng -ooutput.png input.dot@
dotify :: (Ord k, Show k, Show v) => Trie k v -> String
dotify trie = concat
              [ "digraph {\n"
              , evalState (goTrie trie) 0
              , "}\n"
              ]
  where
    goTrie :: (Ord k, Show k, Show v) => Trie k v -> State Int String
    goTrie t = do
      n <- get
      modify' (1 +)
      rest <- mapM (goBranch n) (M.toList $ children t)
      return $ concat
        [ "n", show n, " [ label = \"", show (value t), "\""
        , if end t then ", peripheries = 2" else ""
        , " ];\n", concat rest
        ]

    goBranch :: (Ord k, Show k, Show v) => Int -> (k, Trie k v) -> State Int String
    goBranch c (k, t) = do
      n <- get
      rest <- goTrie t
      return $ concat
        [ "n", show c, " -> n", show n, " [ label = \"", show k, "\" ];\n"
        , rest
        ]

-- | The same as 'dotify' but handles branches labelled with 'Char's
-- in a more readable way.
dotify' :: Show v => Trie Char v -> String
dotify' = dotify . go
  where
    go t = t { children = M.mapKeysMonotonic PrettyChar . M.map go $ children t }
