-- |

module HtmlToken where

import Text.HTML.Parser
import Text.HTML.Tree
import Text.HTML.Utils
import Data.Text qualified as T
import Data.Text (Text)

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
