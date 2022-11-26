{-# LANGUAGE InstanceSigs #-}

module OrgMode where

import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T

data Header = H2
    deriving (Show)
data TodoStatus = Todo | Done | InProgress
    deriving (Show)
newtype Priority = Priority Text
    deriving (Show)

data OrgModeNode = OrgModeNode
    { header :: Header
    , todoStatus :: TodoStatus
    , title :: Text
    , priority :: Maybe Priority
    , tags :: [Text]
    , closedAt :: Maybe Text
    , contents :: [Text]
    }
    deriving (Show)

class ToDisplay a where
    toDisplay :: a -> Text

instance ToDisplay a => ToDisplay (Maybe a) where
    toDisplay :: ToDisplay a => Maybe a -> Text
    toDisplay Nothing = ""
    toDisplay (Just a) = toDisplay a

instance ToDisplay Header where
    toDisplay :: Header -> Text
    toDisplay H2 = "** "

instance ToDisplay TodoStatus where
    toDisplay :: TodoStatus -> Text
    toDisplay Todo = "TODO "
    toDisplay Done = "DONE "
    toDisplay InProgress = "InProgress "

instance ToDisplay Priority where
    toDisplay :: Priority -> Text
    toDisplay (Priority t) = "[#" <> t <> "] "

instance ToDisplay OrgModeNode where
    toDisplay :: OrgModeNode -> Text
    toDisplay OrgModeNode{..} =
        T.unlines $
            [ toDisplay header
                <> toDisplay todoStatus
                <> toDisplay priority
                <> title
                <> (if null tags then "" else "   :" <> T.unwords [t <> ":" | t <- tags])
            ]
                ++ ( case closedAt of
                        Just c -> ["CLOSED: [" <> c <> "]"]
                        Nothing -> []
                   )
                ++ ["- " <> c | c <- contents]
