{-# LANGUAGE TemplateHaskell #-}

module ParseBookItemNode where

import Data.ByteString qualified as BS
import Data.Either
import System.Directory
import Utils
import Text.HTML.Utils
import Data.Bifunctor (first, second)
import Data.Text (Text)
import Data.Text qualified as T
import Control.Lens
import Data.Tree
import Text.HTML.Parser
import Text.HTML.Tree
import Types

$( makeLensesFor
    [ ("rootLabel", "_rootLabel")
    , ("subForest", "_subForest")
    ]
    ''Tree
 )

makePrisms ''Attr
makePrisms ''Token

parseRespToTokens :: Text -> [Token]
parseRespToTokens = parseTokens

getBookListBlock :: [Token] -> [Token]
getBookListBlock ts =
  let as = dropWhile (not . isBookListUl) ts
      bs1 = takeWhile (not . isUlClose) as
      bs2 = dropWhile (not . isUlClose) as
   in bs1 ++ take 1 bs2


bookListLiToBookRead :: Tree Token -> Either (Text, Tree Token) BookRead
bookListLiToBookRead tt = first (,tt) $ do
      (detailPage, title, rating, readAt) <- parseItemShow $ filter (isBookItemShowDiv . rootLabel) $ subForest tt
      (author, comments) <- parseItemHide $ filter (isBookItemHideDiv . rootLabel) $ subForest tt
      pure BookRead{..}

parseItemShow = undefined
-- parseItemShow x = do
--   (detailPage, title) <- parseTitleSection $ filter (isBookTitle . rootLabel) $ subForest x
--   (rating, readAt) <- parseDateSection $ filter (isBookDate . rootLabel) $ subForest x

parseItemHide = undefined

filterTree :: (a -> Bool) -> Tree a -> [a]
filterTree f = foldTree (\t ts -> ( if f t then [t] else []  ) ++ concat ts)

getData = do
  let fname = "./data/read-1.html"
  respBS <- BS.readFile fname
  let allTokens = parseRespToTokens $ bsToText respBS
  let booklistBlock = getBookListBlock allTokens
  case tokensToForest booklistBlock of
    Left err -> print err
    Right fs -> mapM_ print $ filterTree isBookItemShowDiv $ head fs

processFile :: PageNum -> [Either (Text, Tree Token) BookRead] -> IO ()
processFile pnum results = do
  let fname = "./data/read-" <> show pnum <> ".html"
  fexist <- doesFileExist fname
  if fexist
    then do
      respBS <- BS.readFile fname
      let allTokens = parseRespToTokens $ bsToText respBS
      let booklistBlock = getBookListBlock allTokens
      case tokensToForest booklistBlock of
        Left err -> print err
        Right fs -> mapM_ print $ test $ subForest $ head fs
  else
    print results

isBookListUl :: Token -> Bool
isBookListUl = bySelector "ul" "list-view"

isBookListLi :: Token -> Bool
isBookListLi = bySelector "li" "item"

isBookItemShowDiv = bySelector "div" "item-show"
isBookItemHideDiv = bySelector "div" "hide"
isBookTitleDiv = bySelector "div" "title"
isBookDateDiv = bySelector "div" "date"

isGridDateDiv = bySelector "div" "grid-date"
isCommentDiv = bySelector "div" "comment"

bySelector :: TagName -> AttrValue -> Token -> Bool
bySelector tagName cssName t =
    t ^? (_TagOpen . _1) == Just tagName
        && anyOf (_TagOpen . _2 . traverse . _Attr) (hasClass cssName) t

hasClass :: AttrValue -> (AttrName, AttrValue) -> Bool
hasClass classValue (an, av) = an == "class" && classValue `T.isInfixOf` av


-- | When response is like
--
--  @
--  [TagOpen "ul" [Attr "class" "list-view"],ContentText "\n",TagClose "ul"]
--  @
--
-- will imply end of pagination
--
isEndOfPagination :: [Token] -> Bool
isEndOfPagination [TagOpen "ul" _, _, TagClose "ul"] = True
isEndOfPagination _ = False
