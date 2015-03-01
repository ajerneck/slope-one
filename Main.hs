{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Data.Csv
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

import Debug.Trace

type User = T.Text
type Item = T.Text
type Score = Double

-- | Data structures and code for parsing the CSV file.
data Rating = Rating { user :: !User
                     , item :: !Item
                     , score :: !Score
                     } deriving (Show)

type ErrorMsg = String
type CsvData = (Header, V.Vector Rating)

instance FromNamedRecord Rating where
  parseNamedRecord r = Rating <$> r .: "User-ID" <*> r .: "ISBN" <*> r .: "Book-Rating"

myOptions = defaultDecodeOptions {
  decDelimiter = fromIntegral (ord ';')
  }

parseCSV :: FilePath -> IO (Either ErrorMsg CsvData)
parseCSV f = do
  contents <- BL.readFile f
  return $ decodeByNameWith myOptions contents

-- | Data structures and code for the actual Slope one algorithm.

type ItemRatings = M.Map Item (M.Map User Score)


readRatings :: V.Vector Rating -> ItemRatings
readRatings = M.unionsWith (M.union) . V.toList . V.map (\(Rating u i s) -> M.singleton i (M.singleton u s))



-- calculate deviations.
-- TODO: what datastructure should they be stored in?
devs x = map (\(i,j) -> deviation (M.findWithDefault M.empty i x) (M.findWithDefault M.empty j x)) $ allPairs where
  allPairs = pairs $ M.keys x

-- Calculate deviation between a pair of items.
deviation i j = result where
  -- calculate the deviations as the average deviation between the ratings.
  result = ds/card
  ds = sum $ zipWith (-) (M.elems i') (M.elems j')
  -- restrict ratings to those made by both users.
  i' = M.filterWithKey (\k _ -> k `S.member` commonKeys) i
  j' = M.filterWithKey (\k _ -> k `S.member` commonKeys) j
  -- cardinality is the size of the set of common users.
  card = fromIntegral $ S.size $ commonKeys
  -- make the set of users that rated both items.
  commonKeys = S.intersection (M.keysSet i) (M.keysSet j)

pairs :: (Eq a) => [a] -> [(a, a)]
pairs x = [(i, j) | i <- x, j <- x, j /= i]


-- | Concrete application

bookFile = "/home/alexander/data/data-science/book-crossing/BX-Book-Ratings.csv"
testFile = "testdata.csv"

main = do
  x <- parseCSV testFile
  case x  of
   Left m -> error m
   Right x -> return $ readRatings $ snd x
