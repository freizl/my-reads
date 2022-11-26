{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad
import Data.Aeson
import Data.ByteString qualified as BS
import Data.Either
import Data.Text (Text)
import Data.Text qualified as T
import Text.HTML.Parser
import Text.HTML.Tree
import Data.Text.IO qualified as T
import Data.Tree (Tree (..))
import Douban.Download
import Douban.HtmlToken
import Douban.Types
import Douban.Utils
import System.Directory
import System.Environment
import System.Exit
import Text.Pretty.Simple
import OrgMode

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["download"] -> downloadAll
    ["process"] -> processFile 1 []
    ["org"] -> generateOrgFile
    _ -> error "Unsupported command"

jsonFile :: FilePath
jsonFile = "./data/douban-read.json"

orgFile :: FilePath
orgFile = "./data/douban-read.org"

generateOrgFile :: IO ()
generateOrgFile = do
  eBookRead <- eitherDecodeFileStrict' @[BookRead] jsonFile
  case eBookRead of
    Left err -> print err
    Right books -> T.writeFile orgFile
      $ T.unlines
      $ map ( toDisplay . toOrgModeNode ) books

processFile :: PageNum -> [Either (Text, Tree Token) BookRead] -> IO ()
processFile pnum results = do
  let fname = "./data/read-" <> show pnum <> ".html"
  fexist <- doesFileExist fname
  if fexist
    then do
      respBS <- BS.readFile fname
      let allTokens = parseTokens $ bsToText respBS
      let booklistBlock = getBookListBlock allTokens

      when (null booklistBlock) $ do
        putStrLn "unable to parse book list from this response"
        print respBS
        exitFailure
      case tokensToForest booklistBlock of
        Left err -> print err
        Right fs -> do
          let eBooks = bookUlBlockToBookRead (head fs)
          processFile (pnum + 1) (results ++ eBooks)
    else
      mapM_ pPrint (lefts results)
        >> encodeFile jsonFile (rights results)
