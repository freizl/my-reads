{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Douban.Types where

import Data.Aeson
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import OrgMode

type Url = String

data BookCategory = WantToRead | CurrentlyReading | Read

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

toOrgModeNode :: BookRead -> OrgModeNode
toOrgModeNode BookRead{..} =
  OrgModeNode
    { header = H2
    , todoStatus = Done
    , title = title <> " by " <> author
    , priority = fmap (Priority . T.pack . show) rating
    , tags = [] -- \^ unable to obtain tags
    , closedAt = Just readAt
    , contents =
        ["[[" <> T.pack detailPage <> "][douban link]]"]
          ++ [comments | not (T.null comments)]
    }

toPathParam :: BookCategory -> String
toPathParam WantToRead = "with"
toPathParam CurrentlyReading = "do"
toPathParam Read = "collect"

targetUrl :: BookCategory -> PageNum -> Url
targetUrl bookCategory pnum =
  "https://book.douban.com/people/"
    <> "freizl"
    <> "/"
    <> toPathParam bookCategory
    <> "?sort=time&filter=all&mode=list&tags_sort=count"
    <> "&start="
    ++ show ((unPageNum pnum - 1) * 30)
