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


data CmdLine = CmdLine
    { outputFilename :: Maybe FilePath
    , inputFilename  :: FilePath
    } deriving (Read, Eq, Show)


cmdLine :: Parser CmdLine
cmdLine = CmdLine
    <$> optional (strOption $ long "output" <> short 'o' <> metavar "OUTPUT-FILE")
    <*> argument str (metavar "INPUT-FILE")

outputFile :: CmdLine -> FilePath
outputFile (CmdLine Nothing i)  = i ++ ".trie.gz"
outputFile (CmdLine (Just o) _) = o

main:: IO ()
main = execParser parser >>= \c -> do
  hInput <- openFile (inputFilename c) ReadMode
  wordlist <- map T.toStrict . T.words <$> TI.hGetContents hInput
  let sk  = skeleton wordlist
      out = outputFile c
  B.writeFile out . compress $ encode sk
  where
    parser = info (helper <*> cmdLine) mempty
