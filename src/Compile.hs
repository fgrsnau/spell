{-# LANGUAGE Trustworthy #-}

-- |
-- Module      : Main
-- Description : Compiler for wordlists.
-- Copyright   : (c) Stefan Haller, 2014
--
-- License     : MIT
-- Maintainer  : s6171690@mail.zih.tu-dresden.de
-- Stability   : experimental
-- Portability : portable
--
-- This program compiles the wordlist files for usage with the “spell” program.
--
-- It loads the input file and extracts all words. For this list of words a
-- 'Data.Trie.skeleton' is created. This 'Trie' gets binary encoded, compressed
-- and written to the output file.
--
-- If the user specifies no output file, the extension “.trie.gz” is appended to
-- the input filename.
module Main where

import           Codec.Compression.GZip (compress)

import           Data.Binary (encode)
import qualified Data.ByteString.Lazy as B
import           Data.ListLike.Instances ()
import           Data.Monoid (mempty)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TI
import           Data.Trie (skeleton)

import           Options.Applicative

import           System.IO (IOMode (ReadMode), openFile)


-- | Represents all command line flags and arguments.
data CmdLine = CmdLine
    { outputFilename :: Maybe FilePath
    , inputFilename  :: FilePath
    } deriving (Read, Eq, Show)


-- | The command line parser.
cmdLine :: Parser CmdLine
cmdLine = CmdLine
    <$> optional (strOption $ long "output" <> short 'o' <> metavar "OUTPUT-FILE")
    <*> argument str (metavar "INPUT-FILE")

-- | Calculates the output file. Appends a file extension to input
-- filename if no output file was given. Otherwise the output filename
-- is used as is.
outputFile :: CmdLine -> FilePath
outputFile (CmdLine Nothing i)  = i ++ ".trie.gz"
outputFile (CmdLine (Just o) _) = o

-- | Reads the given input file, extracts all words and builds a
-- 'skeleton'. This skeleton is then encoded as a binary stream,
-- compressed and written to the output file.
main:: IO ()
main = execParser parser >>= \c -> do
  hInput <- openFile (inputFilename c) ReadMode
  wordlist <- map T.toStrict . T.words <$> TI.hGetContents hInput
  let sk  = skeleton wordlist
      out = outputFile c
  B.writeFile out . compress $ encode sk
  where
    parser = info (helper <*> cmdLine) mempty
