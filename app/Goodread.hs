{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Applicative
import Data.ByteString.Lazy qualified as BSL
import Data.Char
import Data.Csv
import Data.ISBN
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import OrgMode
import Text.Pretty.Simple

data Book = Book
    { bid :: Text
    , title :: Text
    , author :: Text
    , authorLF :: Text
    , additionalAuthors :: Maybe Text
    , isbn :: Either Field ISBN
    , isbn13 :: Either Field ISBN
    , myRating :: Int
    , averageRating :: Float
    , publisher :: Text
    , binding :: Text
    , numberOfPages :: Maybe Int
    , yearPublished :: Maybe Int
    , originalPubilshYear :: Maybe Int
    , dateRead :: Maybe Text
    , dateAdded :: Text
    , bookshelves :: Maybe Text
    , bookshelvesWithPosition :: Maybe Text
    , exclusiveShelf :: Maybe Text
    , myReview :: Maybe Text
    , spoiler :: Maybe Text
    , privateNotes :: Maybe Text
    , readCount :: Int
    , ownedCopies :: Int
    }
    deriving (Show)

instance FromField ISBN where
    parseField :: Field -> Parser ISBN
    parseField s =
        let str =
                T.takeWhile isDigit $
                    T.dropWhile (not . isDigit) $
                        T.decodeUtf8 s
         in case validateISBN str of
                Left _ -> empty
                Right r -> pure r

instance FromNamedRecord Book where
    parseNamedRecord m =
        Book
            <$> m
            .: "Book Id"
            <*> m
            .: "Title"
            <*> m
            .: "Author"
            <*> m
            .: "Author l-f"
            <*> m
            .: "Additional Authors"
            <*> m
            .: "ISBN"
            <*> m
            .: "ISBN13"
            <*> m
            .: "My Rating"
            <*> m
            .: "Average Rating"
            <*> m
            .: "Publisher"
            <*> m
            .: "Binding"
            <*> m
            .: "Number of Pages"
            <*> m
            .: "Year Published"
            <*> m
            .: "Original Publication Year"
            <*> m
            .: "Date Read"
            <*> m
            .: "Date Added"
            <*> m
            .: "Bookshelves"
            <*> m
            .: "Bookshelves with positions"
            <*> m
            .: "Exclusive Shelf"
            <*> m
            .: "My Review"
            <*> m
            .: "Spoiler"
            <*> m
            .: "Private Notes"
            <*> m
            .: "Read Count"
            <*> m
            .: "Owned Copies"

toOrgSection :: Book -> Text
toOrgSection Book{..} =
    toDisplay $
        OrgModeNode
            { header = H2
            , todoStatus = Done
            , title = title <> " by " <> author
            , priority = if myRating > 0 then (Just . Priority . T.pack . show $ myRating) else Nothing
            , tags = case bookshelves of
                Just t -> [t]
                Nothing -> []
            , closedAt = fmap (T.replace "/" "-") dateRead
            , contents =
                ["[[https://www.goodreads.com/book/show/" <> bid <> "][goodread link]]"]
                    ++ ( case myReview of
                            Just comments -> [T.strip c | c <- T.splitOn "<br/>" comments]
                            Nothing -> []
                       )
            }

main :: IO ()
main = do
    resp <- BSL.readFile "./data/goodreads_library_export.csv"
    let ebooks = decodeByName @Book resp
    case ebooks of
        Left err -> print err
        Right (_, books) -> do
            -- playWithBooks books
            generateOrgFile $
                V.filter ((== Just "read") . exclusiveShelf) books

generateOrgFile :: Vector Book -> IO ()
generateOrgFile bs =
    T.writeFile "./data/goodread.org" $
        T.unlines $
            V.toList $
                V.map toOrgSection bs

playWithBooks :: Vector Book -> IO ()
playWithBooks bs = do
    -- books without year of published
    V.mapM_ pPrint $
        V.take 2 $
            V.filter (isJust . myReview) $
                V.filter ((== Just "read") . exclusiveShelf) bs
