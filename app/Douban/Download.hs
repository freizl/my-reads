module Douban.Download where

import Control.Concurrent
import Data.ByteString qualified as BS
import Data.String (fromString)
import Douban.HtmlToken
import Douban.Types
import Douban.Utils
import Text.HTML.Parser
import Network.HTTP.Simple

downloadAll :: IO ()
downloadAll = downloadReadHistory 1

downloadReadHistory :: PageNum -> IO ()
downloadReadHistory pnum = do
  putStrLn ("Process page " <> show pnum)
  respBS <- crawlPage Read pnum
  let allTokens = parseTokens $ bsToText respBS
  let booklistBlock = getBookListBlock allTokens
  let filename = "./data/read-" <> show pnum <> ".html"
  if isEndOfPagination booklistBlock
    then pure ()
    else
      BS.writeFile filename respBS
        >> putStrLn ("write to " <> filename)
        >> threadDelay (2 * 10 ^ (6 :: Int))
        >> downloadReadHistory (pnum + 1)

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
