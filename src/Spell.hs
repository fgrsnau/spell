{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}

-- |
-- Module      : Main
-- Description : The spell checkers’ heart.
-- Copyright   : (c) Stefan Haller, 2014
--
-- License     : MIT
-- Maintainer  : s6171690@mail.zih.tu-dresden.de
-- Stability   : experimental
-- Portability : portable
--
-- This applications performs spell checking by calculating the minimum edit
-- distance to each of the words in its wordlist.
--
-- The wordlist is a 'Trie' 'Data.Trie.skeleton' which is loaded from a binary
-- encoded and compressed file. The files can be produced with the
-- “spell-compile” program.
--
-- Only alphabetic characters are checked for corrections. All other characters
-- are written to the output file without any modifications. The machinery which
-- allows this are the functions around the 'Annotation' data type.
module Main where

import           Codec.Compression.GZip (decompress)

import           Control.Applicative
import           Control.Monad

import           Data.Binary (decode)
import qualified Data.ByteString.Lazy as B
import           Data.List (groupBy)
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as TS
import qualified Data.Text.IO as TSI
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLI
import           Data.Trie (Trie, toList)

import           Options.Applicative
import           Options.Applicative.Help.Chunk

import           Spell.Confusion (confusionPenalties)
import           Spell.Edit (Penalties, bestEdits', defaultPenalties)

import           System.IO


-- * Annotations

-- | Annotates a value: Either keep the value or consider it for replacement.
--
-- At the type level this is a something like @'Either' a a@.
data Annotation a = Consider { unAnnotation :: a }
                  | Keep     { unAnnotation :: a }
                  deriving (Functor, Read, Eq, Show)

-- | True if both annotation are of the same type.
sameAnnotation :: Annotation a -> Annotation a -> Bool
sameAnnotation (Consider _) (Consider _) = True
sameAnnotation (Keep     _) (Keep     _) = True
sameAnnotation           _            _  = False

-- | Returns the function which is able to construct a annotation of the same
-- type.
annotationConstr :: Annotation a -> (b -> Annotation b)
annotationConstr (Consider _) = Consider
annotationConstr (Keep     _) = Keep

-- | Annotates each 'Char' in the 'String'. All alphabetical characters are
-- “considered” and the rest is “kept”.
annotate :: (Char -> Bool) -> String -> [Annotation Char]
annotate p = map f
  where
    f x
      | p x       = Consider x
      | otherwise = Keep x

-- | Merges subsequent annotations of the same type.
mergeAnnotation :: [Annotation Char] -> [Annotation String]
mergeAnnotation = map f . groupBy sameAnnotation
  where
    f :: [Annotation Char] -> Annotation String
    f xs = annotationConstr (head xs) (map unAnnotation xs)


-- | Represents all command line flags and arguments.
data CmdLine = CmdLine
    { wordlistFilename :: FilePath
    , inputFilename    :: FilePath
    , outputFilename   :: FilePath
    , batch            :: Bool
    , useConfusion     :: Bool
    , cutoff           :: Maybe Double
    } deriving (Read, Eq, Show)


-- * Command Line Parsing and Processing

-- | The command line parser.
cmdLine :: Parser CmdLine
cmdLine = CmdLine
    <$> strOption (short 'w'
                   <> long "wordlist"
                   <> metavar "FILE"
                   <> help "Specifies the wordlist file.")
    <*> strOption (short 'i'
                   <> long "input"
                   <> metavar "FILE"
                   <> help "Specifies the input file.")
    <*> strOption (short 'o'
                   <> long "output"
                   <> metavar "FILE"
                   <> help "Specifies the output file.")
    <*> switch    (short 'b'
                   <> long "batch"
                   <> help "Enables batch mode.")
    <*> switch    (short 'u'
                   <> long "use-confusion"
                   <> help "Uses more sophisticated transition matrices.")
    <*> optional  (option (short 'c'
                           <> long "cut-off"
                           <> metavar "NUMBER"
                           <> help "Specifies cut-off as double value."))

-- | Loads the 'Data.Trie.skeleton' from file.
loadSkeleton :: CmdLine -> IO (Trie Char ())
loadSkeleton CmdLine { wordlistFilename = f } = do
  content <- B.readFile f
  return . decode $ decompress content

-- | Loads the input file and annotates the 'TS.Text'.
loadInput :: (Char -> Bool) -> CmdLine -> IO [Annotation TS.Text]
loadInput p CmdLine { inputFilename = f } = do
  handle <- openFile f ReadMode
  content <- TLI.hGetContents handle
  return $ annotateText p content

-- | Returns the penalty functions according to command line flags.
penalties :: CmdLine -> Penalties Char Double
penalties CmdLine { useConfusion = c }
  | c         = confusionPenalties
  | otherwise = defaultPenalties

-- * Main Functionality of the Program

-- | Takes a 'Trie' and returns a function which will return True if
-- the character is available in the Trie.
triePredicate :: Ord k => Trie k v -> k -> Bool
triePredicate t = flip S.member s
 where
   s = S.fromList . concat . map fst $ toList t

-- | Annotates each word in the 'TS.Text'.
annotateText :: (Char -> Bool) -> TL.Text -> [Annotation TS.Text]
annotateText p = map (fmap TS.pack) . mergeAnnotation . annotate p . TL.unpack

-- | Processes each annotation. All values which should be kept are written to
-- the output handle as is. All other values are mangled through the function in
-- the first argument and then written to the handle.
processAnnotation :: (TS.Text -> IO TS.Text) -> Handle -> Annotation TS.Text -> IO ()
processAnnotation _ h (Keep t) = TSI.hPutStr h t
processAnnotation f h (Consider t) = f t >>= TSI.hPutStr h

-- | Determines the replacement for the given word and suggestion list. The
-- first argument specifies if batch mode is enabled.
determine :: Bool -> TS.Text -> [TS.Text] -> IO TS.Text
determine True  w ss = return . fromMaybe w $ listToMaybe ss
determine False w ss = case ss of
  []           -> ask 0     -- No suggestions, ask user for new word.
  s:_ | w == s -> return w  -- The first suggestion is original word, so don’t ask.
  _            -> ask 5     -- Otherwise display suggestions and ask.
  where
    ask n = do
      input <- askUser w (take n ss)
      case input of
        Nothing -> ask (n + 10)
        Just w' -> verbose w w' >> return w'

-- | Presents the user the current word and a list of suggestions. The user must
-- choose the replacement.
--
-- Returns 'Nothing' if the user asks for more suggestions. Otherwise returns
-- the replacement.
--
-- The user can input the following:
--
--   * Number: Chooses the corresponding word.
--
--   * “?”: Requests more suggenstions.
--
--   * Empty string: The word is not replaced.
--
--   * Every other string @x@: The word is replaced by @x@.
askUser :: TS.Text -> [TS.Text] -> IO (Maybe TS.Text)
askUser w ss = do
  TSI.putStr "Current word: "
  TSI.putStrLn w
  TSI.putStrLn "Suggestions:"
  when (null ss) $ TSI.putStrLn "  (No suggestions.)"
  forM_ suggestions $ \(i, s) -> do
    TSI.putStr "  "
    TSI.putStr i
    TSI.putStr ") "
    TSI.putStrLn s
  TSI.putStr "Your choice: "
  hFlush stdout
  w' <- TSI.getLine
  case w' of
    ""  -> return $ Just w
    "?" -> return Nothing
    _   -> return $ lookup w' suggestions <|> Just w'
  where
    numbers = map (TS.pack . show) [1 :: Int ..]
    suggestions = zip numbers ss

-- | Prints the result of the choosen action. The first parameter is the current
-- word and the second parameter is the replacement.
verbose :: TS.Text -> TS.Text -> IO ()
verbose w w'
  | w == w'   = do
    TSI.putStr "Did not replace “"
    TSI.putStr w
    TSI.putStrLn "”\n"
  | otherwise = do
    TSI.putStr "Replaced “"
    TSI.putStr w
    TSI.putStr "” by “"
    TSI.putStr w'
    TSI.putStrLn "”\n"

-- | The main entry point for the application. Parses the command line argument,
-- loads the wordlist file, loads the input file and then processes each of the
-- annotated words.
main :: IO ()
main = execParser parserInfo >>= \c -> do
  skel <- loadSkeleton c
  inputs <- loadInput (triePredicate skel) c
  let f w = determine (batch c) w $ bestEdits' (penalties c) (cutoff c) w skel
  withFile (outputFilename c) WriteMode $
    \h -> forM_ inputs $ processAnnotation f h
  where
    parserInfo = (info (helper <*> cmdLine) fullDesc) { infoFooter = desc }

    desc = vsepChunks $ map (paragraph . concat)
           [ [ "This spell checker program will read the input file and write "
             , "the corrected content to the given output file. It will only "
             , "consider alphabetical characters and every other character is "
             , "written unmodified to the output file."
             ]
           , [ "If batch mode is enabled, the program will automatically "
             , "correct the text with the best suggestion it has found. This "
             , "mode is not interactive."
             ]
           , [ "If batch mode is disabled, the program will ask the user "
             , "for a replacement for every misspelled word. All correctly "
             , "spelled words are written to the output file without further "
             , "processing."
             ]
           , [ "The program expects the wordlist file to be the output of the "
             , "“spell-compile” program. The file should contain the binary "
             , "representation of a pre-compiled “Trie”."
             ]
           , [ "To use a more sophisticated transition matrix, pass the "
             , "“--use-confusion” flag."
             ]
           , [ "Additional you can speed up the program by using the "
             , "“--cut-off” parameter. This will remove all children for "
             , "nodes where the minimal minimum edit distance exceeds some "
             , "value. This might change the list of suggestions and the "
             , "result might get wrong. Too high values will lead to more "
             , "evaluation of the Trie and the program will actually take "
             , "longer. A good cut-off value is 2 if you can make sure that "
             , "the text will probably not contain more than 2 typos per "
             , "word."
             ]
           , [ "During interactive non-batch mode the user has the following "
             , "options: The user can decide to leave the word unmodified "
             , "and press ENTER. To choose one of the suggestions, he can "
             , "insert its number and confirm with ENTER. If the user inputs "
             , "a question mark (“?”) the program will display more "
             , "suggestions. The last possibility is to enter a custom "
             , "replacement which is literally written to the output file."
             ]
           ]
