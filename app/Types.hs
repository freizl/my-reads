{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Data.Aeson
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics

newtype PageNum = PageNum {unPageNum :: Int}
  deriving newtype (Show, Num)

data BookRead = BookRead
  { title :: Text
  , author :: Text
  , readAt :: Text
  , comments :: Text
  , detailPage :: Url
  , rating :: Maybe Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON BookRead

instance ToJSON BookRead

toOrgSection :: BookRead -> Text
toOrgSection BookRead{..} =
  T.unlines $
    [ "** DONE "
        <> ( case rating of
              Just r -> "[#" <> (T.pack $ show r) <> "] "
              Nothing -> ""
           )
        <> title
        <> " by "
        <> author
    , "CLOSED: ["
        <> readAt
        <> "]"
    , "- " <> "[[" <> T.pack detailPage <> "][douban link]]"
    ]
      ++ (if T.null comments then [] else ["- " <> comments])

data BookCategory = WantToRead | CurrentlyReading | Read

toDoubanSubPath :: BookCategory -> String
toDoubanSubPath WantToRead = "with"
toDoubanSubPath CurrentlyReading = "do"
toDoubanSubPath Read = "collect"

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
