{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad
import Data.Aeson
import Data.Bifunctor (first, second)
import Data.ByteString qualified as BS
import Data.Char (isDigit)
import Data.Either
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Tree (Tree (..))
import Download
import HtmlToken
import System.Directory
import System.Environment
import System.Exit
import Text.HTML.Parser
import Text.HTML.Tree
import Text.Pretty.Simple
import Types
import Utils

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
    Right books -> T.writeFile orgFile $ T.unlines (map toOrgSection books)

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
        >> encodeFile jsonFile (rights results)

{- | Given following Tree Token and generate BookRead

 @
 li class="item"
   div class="item-show"
   div class="hide"
 @
-}
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
   in if length ys == 3
        then
          let linkEl = ys !! 1
              linkToken = rootLabel linkEl
              titleContent = rootLabel (head (subForest linkEl))
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
  if null (subForest x)
    then Left "date section has no children"
    else
      let ys = subForest x
          (mRatingToken, dateToken) =
            if length ys == 3
              then (Just (rootLabel (ys !! 1)), rootLabel (ys !! 2))
              else (Nothing, rootLabel (head ys))
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
  parseGridDate (x1 : _) =
    let introEl = subForest x1 !! 1
     in case subForest introEl of
          [Node (ContentText t) _] -> Right $ T.strip $ head $ T.splitOn "/" t
          _ -> Left "no intro found"

  parseComments :: [Tree Token] -> Text
  parseComments [] = ""
  parseComments (x2 : _) = case subForest x2 of
    [Node (ContentText t) _] -> T.strip t
    _ -> ""

-- | parse string "rating5-t"
parseRatingString :: Text -> Either Text Int
parseRatingString t = case take 1 $ drop 6 $ T.unpack t of
  [x] -> if isDigit x then Right (read [x]) else Left (T.pack $ "unable to parse char " <> [x])
  _ -> Left ("not known rating string " <> t)
