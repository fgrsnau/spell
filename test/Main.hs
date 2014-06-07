module Main where

import Data.Trie.Test (trieTest)
import Spell.Test (spellTest)
import Test.Hspec (hspec, parallel)

main :: IO ()
main = hspec $ parallel (trieTest >> spellTest)
