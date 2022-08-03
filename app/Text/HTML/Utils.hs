-- |
module Text.HTML.Utils where

import Text.HTML.Parser

getBody :: [Token] -> [Token]
getBody ts =
  let withoutHeader = dropWhile (not . isBodyOpen) ts
      onlyBody = takeWhile (not . isBodyClose) withoutHeader
      rest = dropWhile (not . isBodyClose) withoutHeader
   in onlyBody ++ take 1 rest

isBodyOpen (TagOpen "body" _) = True
isBodyOpen _ = False

isBodyClose (TagClose "body") = True
isBodyClose _ = False

isUlClose :: Token -> Bool
isUlClose (TagClose "ul") = True
isUlClose _ = False
