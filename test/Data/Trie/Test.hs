{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
module Data.Trie.Test where

import           Prelude hiding (lookup)

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Trie
import           Data.Trie.Internal

import           Test.Hspec
import           Test.QuickCheck


type TrieType = Trie Char ()


instance (Arbitrary k, Ord k) => Arbitrary (Trie k ()) where
  arbitrary = do
    ls <- arbitrary :: Arbitrary k => Gen [[k]]
    return (skeleton ls)


buildFunc :: [k] -> [Int] -> Int
buildFunc _ (x1:x2:_) = x1 + x2
buildFunc _ (x1:_)    = x1
buildFunc _ []        = 0

trieTest :: Spec
trieTest = describe "Data.Trie" $ do
  describe "lookup" $ do
    it "returns previously inserted element" $ property $ \k t ->
      let right = lookup k $ insert (\_ _ -> ()) (k :: String) t
      in  Just () == right

    it "returns Nothing for previously deleted element" $ property $ \k t ->
      let right = lookup k $ delete (k :: String) (t :: TrieType)
      in  Nothing == right

  describe "cut" $ do
    it "returns empty Trie if everything is cutted" $ property $ \t ->
      cut (const True) (t :: TrieType) == t { children = M.empty }

    it "does not modify Trie if nothing is cutted" $ property $ \t ->
      cut (const False) t == (t :: TrieType)

  describe "fromList / toList" $ do
    it "forms identity (1)" $ property $ \t ->
      let left  = fromList (\_ _ -> ()) . (id :: [String] -> [String]) . map fst $ toList t
          right = t :: TrieType
      in  left == right

    it "forms identity (2)" $ property $ \ls ->
      let left  = map fst . toList $ fromList (\ _ _ -> ()) ls
          right = S.toList $ S.fromList ls
          _     = ls :: [String]
      in  left == right
          
  describe "skeleton + populate" $ do
    it "returns the same result as fromList" $ property $ \ls ->
      let left  = fromList buildFunc ls
          right = populate buildFunc (skeleton ls)
          _     = ls :: [String]
      in  left == right

  describe "expandPaths" $ do
    it "does the right thing™" $ property $ \ t ->
      let f (p1, (p2, _)) = p1 == (p2 :: String)
      in all f . toList $ expandPaths (t :: TrieType)
