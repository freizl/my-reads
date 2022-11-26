module Douban.HtmlToken where

import Data.Bifunctor (first, second)
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree (..))
import Douban.Types
import Text.HTML.Parser
import Text.HTML.Utils

bookUlBlockToBookRead :: Tree Token -> [Either (Text, Tree Token) BookRead]
bookUlBlockToBookRead t =
    map tokenLiToBookRead $
        filter (isBookListLi . rootLabel) $
            subForest t

{- | Given following Tree Token and generate BookRead

 @
 li class="item"
   div class="item-show"
   div class="hide"
 @
-}
tokenLiToBookRead :: Tree Token -> Either (Text, Tree Token) BookRead
tokenLiToBookRead tt = first (,tt) $ do
    (detailPage, title) <- parseTitleSection $ filterTree isBookTitleElem  tt
    (rating, readAt) <- parseDateSection $ filterTree isBookDateElem  tt
    author <- parseGridDate $ filterTree isGridDateElem tt
    comments <- Right $ parseComments $ filterTree isCommentElem tt
    pure BookRead{..}

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

getBookListBlock :: [Token] -> [Token]
getBookListBlock ts =
    let as = dropWhile (not . isBookListUl) ts
        bs1 = takeWhile (not . isUlClose) as
        bs2 = dropWhile (not . isUlClose) as
     in bs1 ++ take 1 bs2

isBookListUl :: Token -> Bool
isBookListUl = bySelector "ul" "list-view"

isBookListLi :: Token -> Bool
isBookListLi = bySelector "li" "item"

isBookItemShowElem :: Token -> Bool
isBookItemShowElem = bySelector "div" "item-show"

isBookTitleElem :: Token -> Bool
isBookTitleElem = bySelector "div" "title"

isBookDateElem :: Token -> Bool
isBookDateElem = bySelector "div" "date"

isBookItemHideElem :: Token -> Bool
isBookItemHideElem = bySelector "div" "hide"

isCommentElem :: Token -> Bool
isCommentElem = bySelector "div" "comment"

isGridDateElem :: Token -> Bool
isGridDateElem = bySelector "div" "grid-date"

bySelector :: TagName -> AttrValue -> Token -> Bool
bySelector name av t =
    case t of
        TagOpen tn attrs -> tn == name && hasClassAttrs av attrs
        _ -> False

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


filterTree :: (a -> Bool) -> Tree a -> [Tree a]
filterTree f t@(Node rl xs) =
  if f rl then [t] else []
  ++ concatMap (filterTree f) xs
