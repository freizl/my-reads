{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Concurrent
import Control.Monad
import Data.Aeson
import Data.Bifunctor (first, second)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Char (isDigit)
import Data.Either
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Tree (Tree (..))
import Data.Tree.Ext
import GHC.Generics
import Network.HTTP.Simple
import System.Directory
import System.Environment
import System.Exit
import Text.HTML.Parser
import Text.HTML.Tree
import Text.HTML.Utils
import Text.Pretty.Simple

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["download"] -> downloadAll
    ["process"] -> processFile 1 []
    _ -> error "Unsupported command"

processFile :: PageNum -> [Either (Text, Tree Token) BookRead] -> IO ()
processFile pnum results = do
  let fname = "./data/read-" <> show pnum <> ".html"
  fexist <- doesFileExist fname
  if fexist
    then do
      respBS <- BS.readFile fname
      let allTokens = parseRespToTokens $ bsToText respBS
      let booklistBlock = getBookListBlock allTokens

      when (null booklistBlock) $ do
        putStrLn "unable to parse book list from this response"
        print respBS
        exitFailure
      case tokensToForest booklistBlock of
        Left err -> print err
        Right fs -> do
          let t1 = head fs
          let eBooks = map tokenLiToBookRead (filter (isBookListItem . rootLabel) $ subForest t1)
          processFile (pnum + 1) (results ++ eBooks)
    else
      mapM_ pPrint (lefts results)
        >> encodeFile "./data/read.json" (rights results)

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
  , detailPage :: Url
  , rating :: Maybe Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON BookRead

data BookCategory = WantToRead | CurrentlyReading | Read

toDoubanSubPath :: BookCategory -> String
toDoubanSubPath WantToRead = "with"
toDoubanSubPath CurrentlyReading = "do"
toDoubanSubPath Read = "collect"

tokenLiToBookRead :: Tree Token -> Either (Text, Tree Token) BookRead
tokenLiToBookRead tt
  | isBookListItem (rootLabel tt) = first (,tt) $ do
      (detailPage, title, rating, readAt) <- parseItemShow $ filter (isBookItemShow . rootLabel) $ subForest tt
      (author, comments) <- parseItemHide $ filter (isBookItemHide . rootLabel) $ subForest tt
      pure BookRead{..}
  | otherwise = Left ("non-book list item", tt)

parseItemShow :: [Tree Token] -> Either Text (Url, Text, Maybe Int, Text)
parseItemShow [] = Left "expect book-item-show token but nothing"
parseItemShow (x : _) = do
  (detailPage, title) <- parseTitleSection $ filter (isBookTitle . rootLabel) $ subForest x
  (rating, readAt) <- parseDateSection $ filter (isBookDate . rootLabel) $ subForest x
  pure (detailPage, title, rating, readAt)

parseTitleSection :: [Tree Token] -> Either Text (Url, Text)
parseTitleSection [] = Left "expects title section but got nothing"
parseTitleSection (x : _) =
  let ys = subForest x
   in if (length ys == 3)
        then
          let linkEl = ys !! 1
              linkToken = rootLabel linkEl
              titleContent = rootLabel ((subForest linkEl) !! 0)
           in (,)
                <$> ( case linkToken of
                        TagOpen "a" attrs -> Right $ head $ [T.unpack av | Attr an av <- attrs, an == "href"]
                        _ -> Left ("expect a link but got " <> T.pack (show linkToken))
                    )
                <*> ( case titleContent of
                        ContentText t -> Right (T.strip t)
                        _ -> Left ("expects content but got " <> T.pack (show titleContent))
                    )
        else Left "expects page link element but got nothing"

parseDateSection :: [Tree Token] -> Either Text (Maybe Int, Text)
parseDateSection [] = Left "expects date section but none"
parseDateSection (x : _) =
  if length (subForest x) == 0
    then Left "date section has no children"
    else
      let ys = subForest x
          (mRatingToken, dateToken) =
            if length ys == 3
              then (Just (rootLabel (ys !! 1)), rootLabel (ys !! 2))
              else (Nothing, rootLabel (ys !! 0))
       in (,)
            <$> ( case mRatingToken of
                    Just ratingToken -> second Just $ case ratingToken of
                      TagOpen "span" attrs -> head [parseRatingString av | Attr an av <- attrs, an == "class"]
                      _ -> Left "expects rating token but got none"
                    Nothing -> Right Nothing
                )
            <*> ( case dateToken of
                    ContentText d -> Right (T.strip $ T.replace "&nbsp;" "" d)
                    _ -> Left "expect date token but got none"
                )

parseItemHide :: [Tree Token] -> Either Text (Text, Text)
parseItemHide [] = Left "expect item hide node but got none"
parseItemHide (x : _) =
  let ys = subForest x
   in (,)
        <$> parseGridDate (filter (isGridDateNode . rootLabel) ys)
        <*> Right (parseComments (filter (isCommentNode . rootLabel) ys))
 where
  parseGridDate :: [Tree Token] -> Either Text Text
  parseGridDate [] = Left "expects grid-date but none"
  parseGridDate (x : _) =
    let introEl = (subForest x) !! 1
     in case subForest introEl of
          [Node (ContentText t) _] -> Right $ T.strip $ head $ T.splitOn "/" t
          _ -> Left "no intro found"

  parseComments :: [Tree Token] -> Text
  parseComments [] = ""
  parseComments (x : _) = case subForest x of
    [Node (ContentText t) _] -> T.strip t
    _ -> ""

-- | parse string "rating5-t"
parseRatingString :: Text -> Either Text Int
parseRatingString t = case (take 1 $ drop 6 $ T.unpack t) of
  [x] -> if isDigit x then Right (read [x]) else Left (T.pack $ "unable to parse char " <> [x])
  _ -> Left ("not known rating string " <> t)

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
isBookListItem (TagOpen "li" xs) = hasClassAttrs "item" xs
isBookListItem _ = False

isBookItemShow :: Token -> Bool
isBookItemShow (TagOpen "div" attrs) = hasClassAttrs "item-show" attrs
isBookItemShow _ = False

isBookTitle :: Token -> Bool
isBookTitle (TagOpen "div" xs) = hasClassAttrs "title" xs
isBookTitle _ = False

isBookDate :: Token -> Bool
isBookDate (TagOpen "div" xs) = hasClassAttrs "date" xs
isBookDate _ = False

isBookItemHide :: Token -> Bool
isBookItemHide (TagOpen "div" attrs) = hasClassAttrs "hide" attrs
isBookItemHide _ = False

isCommentNode :: Token -> Bool
isCommentNode (TagOpen "div" attrs) = hasClassAttrs "comment" attrs
isCommentNode _ = False

isGridDateNode :: Token -> Bool
isGridDateNode (TagOpen "div" attrs) = hasClassAttrs "grid-date" attrs
isGridDateNode _ = False

hasClassAttrs :: Text -> [Attr] -> Bool
hasClassAttrs className = (== 1) . length . filter (hasClass className)

hasClass :: Text -> Attr -> Bool
hasClass className (Attr "class" attrValue) = className `T.isInfixOf` attrValue
hasClass _ _ = False

{- | When response is like

 @
 [TagOpen "ul" [Attr "class" "list-view"],ContentText "\n",TagClose "ul"]
 @

 will imply end of pagination
-}
isEndOfPagination :: [Token] -> Bool
isEndOfPagination [TagOpen "ul" _, _, TagClose "ul"] = True
isEndOfPagination _ = False

---

-- * dummy helpers

---

bsToText :: BS.ByteString -> Text
bsToText = T.decodeUtf8
