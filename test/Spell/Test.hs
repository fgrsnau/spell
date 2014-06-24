{-# LANGUAGE OverloadedStrings #-}
module Spell.Test where

import           Control.Arrow (second)

import           Data.Function (on)
import           Data.List (groupBy, sortBy)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import           Data.Trie

import           Spell.Confusion (confusionPenalties)
import           Spell.Edit

import           System.IO
import           System.IO.Unsafe (unsafePerformIO)

import           Test.Hspec
import           Test.QuickCheck


{-# NOINLINE corpus #-}
corpus :: Trie Char ()
corpus = unsafePerformIO $ do
  h <- openFile "corpus.txt" ReadMode
  c <- TI.hGetContents h
  return . skeleton $ T.words c

spellTest :: Spec
spellTest = describe "Spell.Edit" $ do
  context "with defaultPenalties"   (test defaultPenalties)
  context "with confusionPenalties" (test confusionPenalties)

test :: Penalties Char Double -> Spec
test p = do
  describe "searchBestEdits" $ do
    it "returns 100 first optimal elements in correct order" $ property $ \w ->
      let trie  = shrinkMatrices $ populate (calculateEdit p $ T.pack w) corpus
          left  = searchBestEdits $ expandPaths trie
          right = sortBy (compare `on` snd) . map (second fst) $ toList trie
          f = (==) `on` (map (sortBy (compare `on` fst)) . groupBy ((==) `on` snd))
      in  f left right

    it "corrects “purlpe” to “purple” with costs ≤ 1.0 (checks reversals)" $
      let check ("purple", x) = x <= 1.0
          check _             = False
      in  check . head $ bestEdits p Nothing "purlpe" corpus
