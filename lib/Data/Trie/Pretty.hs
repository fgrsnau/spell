module Data.Trie.Pretty where

import           Prelude hiding (lookup)

import           Control.Monad.Trans.State (State, evalState, get, modify')
import qualified Data.Map as M
import           Data.Trie.Internal (Trie(..))


newtype PrettyChar = PrettyChar Char deriving (Eq, Ord, Read)

instance Show PrettyChar where
  show (PrettyChar c) = [c]


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

dotify' :: Show v => Trie Char v -> String
dotify' = dotify . go
  where
    go t = t { children = M.mapKeysMonotonic PrettyChar . M.map go $ children t }
