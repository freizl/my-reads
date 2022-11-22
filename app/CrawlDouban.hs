{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Concurrent
import Control.Monad
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Tree.Ext
import Network.HTTP.Simple
import System.Directory
import System.Environment
import System.Exit
import Text.HTML.Parser
import Text.HTML.Tree
import Text.HTML.Utils
import Text.Pretty.Simple

main :: IO ()
main = processFile 7

processFile :: PageNum -> IO ()
processFile pnum = do
  let fname = "./data/read-" <> show pnum <> ".html"
  fexist <- doesFileExist fname
  when fexist $ do
    respBS <- BS.readFile fname
    let allTokens = parseRespToTokens $ bsToText respBS
    let booklistBlock = getBookListBlock allTokens

    mapM_ print booklistBlock
    when (null booklistBlock) $ do
      putStrLn "unable to parse book list from this response"
      print respBS
      exitFailure
    case tokensToForest booklistBlock of
      Left err -> print err
      Right fs -> pPrint (drawTree $ head fs) -- expects the forest has size 1
      -- Right fs -> putStrLn ("done " <> show pnum) >> processFile (pnum + 1)


downloadAll :: IO ()
downloadAll = downloadReadHistory 1

downloadReadHistory pnum = do
  putStrLn ("Process page " <> show pnum)
  respBS <- crawlPage Read pnum
  let allTokens = parseRespToTokens $ bsToText respBS
  let booklistBlock = getBookListBlock allTokens
  let filename = "./data/read-" <> show pnum <> ".html"
  if isEndOfPagination booklistBlock
    then pure ()
    else
      BS.writeFile filename respBS
        >> putStrLn ("write to " <> filename)
        >> threadDelay (2 * 10 ^ 6)
        >> downloadReadHistory (pnum + 1)

---

-- * Types

data BookRead = BookRead
  { title :: Text
  , author :: Text
  , readAt :: Text
  , comments :: Text
  }
  deriving (Eq, Show)

data BookCategory = WantToRead | CurrentlyReading | Read

toDoubanSubPath :: BookCategory -> String
toDoubanSubPath WantToRead = "with"
toDoubanSubPath CurrentlyReading = "do"
toDoubanSubPath Read = "collect"

---

-- * Douban

---

type Url = String

type UserCallSign = String

baseUrl :: Url
baseUrl = "https://book.douban.com/people/"

-- TODO: use Query type for query parameter given `start` require change for every request.
--
targetUrl :: BookCategory -> PageNum -> Url
targetUrl bookCategory pnum =
  baseUrl
    <> ("freizl" :: UserCallSign)
    <> "/"
    <> toDoubanSubPath bookCategory
    <> "?sort=time"
    <> "&start="
    ++ show ((unPageNum pnum - 1) * 30)
      <> "&filter=all&mode=list&tags_sort=count"

---

-- * HTTP Client

---

newtype PageNum = PageNum {unPageNum :: Int}
  deriving newtype (Show, Num)

crawlPage ::
  BookCategory ->
  PageNum ->
  IO BS.ByteString
crawlPage bookCategory pnum = do
  let url = targetUrl bookCategory pnum
  print url
  let request = addHeaders (fromString url)
  resp <- httpBS request
  -- print request
  -- print resp
  pure (getResponseBody resp)

addHeaders :: Request -> Request
addHeaders =
  addRequestHeader "Accept" "text/html"
    . addRequestHeader "Accept-Encoding" "gzip"
    . addRequestHeader "Connection" "keep-alive"
    .
    -- Cookie is critical otherwise 403
    addRequestHeader "Cookie" "bid=JPTE-koPHbc;"
    . addRequestHeader "User-Agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:103.0) Gecko/20100101 Firefox/103.0"

---

-- * Deal with TML Tokens

---
parseRespToTokens :: Text -> [Token]
parseRespToTokens = parseTokens

getBookListBlock :: [Token] -> [Token]
getBookListBlock ts =
  let as = dropWhile (not . isBookListBlockOpen) ts
      bs1 = takeWhile (not . isUlClose) as
      bs2 = dropWhile (not . isUlClose) as
   in bs1 ++ take 1 bs2

isBookListBlockOpen :: Token -> Bool
isBookListBlockOpen (TagOpen "ul" [Attr "class" "list-view"]) = True
isBookListBlockOpen _ = False

isBookListItem :: Token -> Bool
isBookListItem (TagOpen "li" xs) = length (filter isItemClass xs) == 1
isBookListItem _ = False

isItemClass :: Attr -> Bool
isItemClass (Attr "class" "item") = True
isItemClass _ = False

-- When response is like
-- [TagOpen "ul" [Attr "class" "list-view"],ContentText "\n",TagClose "ul"]
-- will imply end of pagination

isEndOfPagination :: [Token] -> Bool
isEndOfPagination [TagOpen "ul" _, _, TagClose "ul"] = True
isEndOfPagination _ = False

---

-- * dummy helpers

---

bsToText :: BS.ByteString -> Text
bsToText = T.decodeUtf8
