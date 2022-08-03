module Main where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Tree.Ext
import Network.HTTP.Simple
import System.Environment
import Text.HTML.Parser
import Text.HTML.Tree
import Text.HTML.Utils
import Text.Pretty.Simple

main :: IO ()
main = do
  args <- getArgs
  respBS <- case args of
    ["test"] -> readTestFile
    _ -> crawlPage Read
  let allTokens = parseRespToTokens $ bsToText respBS
  let booklistBlock = getBookListBlock allTokens

  case tokensToForest booklistBlock of
    Left err -> print err
    Right fs -> pPrint (drawTree $ head fs) -- expects the forest has size 1

---

-- * Types

---
data BookCategory = WantToRead | CurrentlyReading | Read

toDoubanSubPath :: BookCategory -> String
toDoubanSubPath WantToRead = "with"
toDoubanSubPath CurrentlyReading = "do"
toDoubanSubPath Read = "collect"

---

-- * Douban

---

type Url = String

baseUrl :: Url
baseUrl = "https://book.douban.com/people/freizl/"

-- TODO: use Query type for query parameter given `start` require change for every request.
--
targetUrl :: BookCategory -> Url
targetUrl bookCategory =
  baseUrl
    <> toDoubanSubPath bookCategory
    <> "?sort=time&start=0&filter=all&mode=list&tags_sort=count"

---

-- * HTTP Client

---

-- TODO: Got 403 forbidden.
-- is douban smart enough to block a bot?
-- or what's wrong in this function?
--
crawlPage :: BookCategory -> IO BS.ByteString
crawlPage bookCategory = do
  let url = targetUrl bookCategory
  let request = addRequestHeader "User-Agent" "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/71.0.3578.98 Safari/537.36" (fromString url)
  print request
  getResponseBody <$> httpBS (fromString url)

readTestFile :: IO BS.ByteString
readTestFile = do
  let testFile = "./data/read-1.html"
  BS.readFile testFile

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

---

-- * dummy helpers

---

bsToText :: BS.ByteString -> Text
bsToText = T.decodeUtf8
