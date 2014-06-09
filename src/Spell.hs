{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Codec.Compression.GZip (decompress)
import           Control.Applicative
import           Control.Monad

import           Data.Binary (decode)
import qualified Data.ByteString.Lazy as B
import           Data.Char (isAlpha)
import           Data.List (groupBy)
import           Data.ListLike.Instances ()
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Monoid
import qualified Data.Text as TS
import qualified Data.Text.IO as TSI
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLI
import           Data.Trie (Trie)

import           Options.Applicative

import           Spell.Confusion (confusionPenalties)
import           Spell.Edit (Penalties, bestEdits', defaultPenalties)

import           System.IO


data Annotation a = Consider { unAnnotation :: a }
                  | Keep     { unAnnotation :: a }
                  deriving (Functor, Read, Eq, Show)

sameAnnotation :: Annotation a -> Annotation a -> Bool
sameAnnotation (Consider _) (Consider _) = True
sameAnnotation (Keep     _) (Keep     _) = True
sameAnnotation           _            _  = False

annotationConstr :: Annotation a -> (b -> Annotation b)
annotationConstr (Consider _) = Consider
annotationConstr (Keep     _) = Keep

annotate :: String -> [Annotation Char]
annotate = map f
  where
    f x
      | isAlpha x = Consider x
      | otherwise = Keep x

mergeAnnotation :: [Annotation Char] -> [Annotation String]
mergeAnnotation = map f . groupBy sameAnnotation
  where
    f :: [Annotation Char] -> Annotation String
    f xs = annotationConstr (head xs) (map unAnnotation xs)


data CmdLine = CmdLine
    { wordlistFilename :: FilePath
    , inputFilename    :: FilePath
    , outputFilename   :: FilePath
    , batch            :: Bool
    , useConfusion     :: Bool
    , cutting          :: Maybe Double
    } deriving (Read, Eq, Show)

cmdLine :: Parser CmdLine
cmdLine = CmdLine
    <$> strOption (short 'w' <> long "wordlist" <> metavar "FILE")
    <*> strOption (short 'i' <> long "input"    <> metavar "FILE")
    <*> strOption (short 'o' <> long "output"   <> metavar "FILE")
    <*> switch    (short 'b' <> long "batch")
    <*> switch    (short 'u' <> long "use-confusion")
    <*> optional  (option (short 'c' <> long "cutting" <> metavar "NUMBER"))

loadSkeleton :: CmdLine -> IO (Trie Char ())
loadSkeleton CmdLine { wordlistFilename = f } = do
  content <- B.readFile f
  return . decode $ decompress content

loadInput :: CmdLine -> IO [Annotation TS.Text]
loadInput CmdLine { inputFilename = f } = do
  handle <- openFile f ReadMode
  content <- TLI.hGetContents handle
  return $ annotateText content

penalties :: CmdLine -> Penalties Char Double
penalties CmdLine { useConfusion = c }
  | c         = confusionPenalties
  | otherwise = defaultPenalties

annotateText :: TL.Text -> [Annotation TS.Text]
annotateText = map (fmap TS.pack) . mergeAnnotation . annotate . TL.unpack

processAnnotation :: (TS.Text -> IO TS.Text) -> Handle -> Annotation TS.Text -> IO ()
processAnnotation _ h (Keep t) = TSI.hPutStr h t
processAnnotation f h (Consider t) = f t >>= TSI.hPutStr h

determineFunc :: Bool                      -- ^ Batch mode?
                 -> Penalties Char Double  -- ^ Penalties to use for MED.
                 -> Maybe Double           -- ^ Just value determines cut off.
                 -> Trie Char ()           -- ^ The 'Data.Trie.skeleton'.
                 -> TS.Text                -- ^ Referenece 'TS.Text'.
                 -> IO TS.Text             -- ^ Determined replacement.
determineFunc True p c s w = return . fromMaybe w . listToMaybe $ bestEdits' p c w s
determineFunc False p c s w = do
  let numbers = map (TS.pack . show) [1 .. 10 :: Int]
      suggestions = zip numbers (bestEdits' p c w s)
  if w == snd (head suggestions)
    then return w
    else do
      TSI.putStr "Current word: "
      TSI.putStrLn w
      TSI.putStrLn "Suggestions:"
      forM_ suggestions $ \(n, s') -> do
        TSI.putStr "  "
        TSI.putStr n
        TSI.putStr ") "
        TSI.putStrLn s'
      TSI.putStr "Your choice: "
      hFlush stdout
      n' <- TSI.getLine
      let new = case lookup n' suggestions of
            Nothing -> if n' == "" then w else n'
            Just s'  -> s'
      TSI.putStr "Replaced “"
      TSI.putStr w
      TSI.putStr "” by “"
      TSI.putStr new
      TSI.putStrLn "”\n"
      return new

main :: IO ()
main = execParser parser >>= \c -> do
  skel <- loadSkeleton c
  inputs <- loadInput c
  let f = determineFunc (batch c) (penalties c) (cutting c) skel
  withFile (outputFilename c) WriteMode $
    \h -> forM_ inputs $ processAnnotation f h
  where
    parser = info (helper <*> cmdLine) mempty
