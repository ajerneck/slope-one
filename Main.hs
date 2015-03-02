{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Data.Csv
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Environment (getArgs)

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



-- Calculate deviations for all pairs of items.
devs x = map (\(i,j) -> (i, j, deviation (M.findWithDefault M.empty i x) (M.findWithDefault M.empty j x) ) ) $ allPairs where
  allPairs = pairs $ M.keys x



-- Store both deviation and cardinality for each pair, because we need both in predict.
data Deviation = Deviation { dev :: Score
                           , card :: Score
                           } deriving Show


-- calculate deviations and cardinality for a pair of items.
deviation i j = Deviation result card where
  -- calculate the deviations as the average deviation between the ratings.
  result = ds/ card
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

-- Make predictions

-- Predict the rating for a user and an item, given all ratings and the deviations between them.
predict user item ir ds = result where
  result = numerator/denominator
  -- sum the cardinality of the item ratings.
  denominator = sum $ M.elems cards
  -- add item deviations to userratings, multiply by the item ratings cardinality, and sum.
  numerator = sum $ M.elems $ M.unionWith (*) cards $ M.unionWith (+) userRatings itemDevs
  -- get the users ratings for other items.
  userRatings = userRatings user ir
  -- get deviations and cardinality for the ratings for the item.
  devCards = itemDeviations item ds
  itemDevs = M.map dev devCards
  cards = M.map card devCards

-- TODO: using zero does not feel completely correct.
-- get all ratings for a user.
userRatings user = M.filter (/=0) . M.mapWithKey (\k a -> M.findWithDefault 0 testUser a)

-- get the deviations for an item.
itemDeviations item = M.fromList . foldr (\(a, b, r) acc -> if a==item then (b,r):acc else acc) []


-- | Concrete application

bookFile = "/home/alexander/data/data-science/book-crossing/BX-Book-Ratings.csv"
testFile = "testdata.csv"

testUser = "ben" :: User
testItem = "whitney" :: Item

libMain = do
  x <- parseCSV testFile
  case x  of
   Left m -> error m
   Right x -> return $ readRatings $ snd x

main = do
  (f:n:_) <- getArgs
  x <- parseCSV f
  case x  of
   Left m -> error m
   Right x -> mapM_ print $ M.toList $ readRatings $ V.take (read n) $ snd x
