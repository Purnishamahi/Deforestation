{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import qualified Data.ByteString.Lazy as BL
import Control.Monad.IO.Class (liftIO)
import Data.List (sortOn, groupBy)
import qualified Data.Text.Lazy as TL
import Data.Aeson (object, (.=))

-- split CSV by comma
splitComma :: String -> [String]
splitComma [] = [""]
splitComma (',' : xs) = "" : splitComma xs
splitComma (x : xs) =
  let (y:ys) = splitComma xs
  in (x:y) : ys

-- recursive aggregation
sumVals :: [Double] -> Double
sumVals = foldl (+) 0

-- read dataset
loadRows :: IO [String]
loadRows = do
  csv <- readFile "Final.csv"
  return (drop 1 (lines csv))

-- group by year
groupYear :: [(Int, Double)] -> [(Int, Double)]
groupYear pairs =
  map (\g -> (fst (head g), sumVals (map snd g)))
      (groupBy (\a b -> fst a == fst b) (sortOn fst pairs))

main :: IO ()
main = scotty 3000 $ do

  ------------------------------------------------
  -- ROOT
  ------------------------------------------------
  get "/" $
    text "Deforestation API Running"

  ------------------------------------------------
  -- DOWNLOAD DATASET
  ------------------------------------------------
  get "/data" $ do
    csv <- liftIO $ BL.readFile "Final.csv"
    setHeader "Content-Type" "text/csv"
    raw csv

  ------------------------------------------------
  -- GLOBAL TOTALS
  ------------------------------------------------
  get "/global-loss" $ do
    rows <- liftIO loadRows
    let vals = map (\r -> read (splitComma r !! 3) :: Double) rows
    text . TL.pack . show . round $ sumVals vals

  get "/global-fire-loss" $ do
    rows <- liftIO loadRows
    let vals = map (\r -> read (splitComma r !! 4) :: Double) rows
    text . TL.pack . show . round $ sumVals vals

  get "/global-emissions" $ do
    rows <- liftIO loadRows
    let vals = map (\r -> read (splitComma r !! 5) :: Double) rows
    text . TL.pack . show . round $ sumVals vals

  ------------------------------------------------
  -- GLOBAL YEARLY DATA
  ------------------------------------------------
  get "/global-loss-yearly" $ do
    rows <- liftIO loadRows
    let pairs =
          map (\r ->
            let c = splitComma r
            in (read (c !! 2) :: Int, read (c !! 3) :: Double)) rows
    let grouped = groupYear pairs
    json [object ["year" .= y, "loss" .= v] | (y,v) <- grouped]

  get "/global-fire-yearly" $ do
    rows <- liftIO loadRows
    let pairs =
          map (\r ->
            let c = splitComma r
            in (read (c !! 2) :: Int, read (c !! 4) :: Double)) rows
    let grouped = groupYear pairs
    json [object ["year" .= y, "fire" .= v] | (y,v) <- grouped]

  get "/global-emissions-yearly" $ do
    rows <- liftIO loadRows
    let pairs =
          map (\r ->
            let c = splitComma r
            in (read (c !! 2) :: Int, read (c !! 5) :: Double)) rows
    let grouped = groupYear pairs
    json [object ["year" .= y, "emissions" .= v] | (y,v) <- grouped]

  ------------------------------------------------
  -- COUNTRY FOREST LOSS
  ------------------------------------------------
  get "/country-loss/:country" $ do
    country <- param "country" :: ActionM String
    rows <- liftIO loadRows
    let filtered = filter (\r -> splitComma r !! 1 == country) rows
    let pairs =
          map (\r ->
            let c = splitComma r
            in (read (c !! 2) :: Int, read (c !! 3) :: Double)) filtered
    let grouped = groupYear pairs
    json [object ["year" .= y, "loss" .= v] | (y,v) <- grouped]

  ------------------------------------------------
  -- COUNTRY FIRE LOSS
  ------------------------------------------------
  get "/country-fire/:country" $ do
    country <- param "country" :: ActionM String
    rows <- liftIO loadRows
    let filtered = filter (\r -> splitComma r !! 1 == country) rows
    let pairs =
          map (\r ->
            let c = splitComma r
            in (read (c !! 2) :: Int, read (c !! 4) :: Double)) filtered
    let grouped = groupYear pairs
    json [object ["year" .= y, "fire" .= v] | (y,v) <- grouped]

  ------------------------------------------------
  -- COUNTRY EMISSIONS
  ------------------------------------------------
  get "/country-emissions/:country" $ do
    country <- param "country" :: ActionM String
    rows <- liftIO loadRows
    let filtered = filter (\r -> splitComma r !! 1 == country) rows
    let pairs =
          map (\r ->
            let c = splitComma r
            in (read (c !! 2) :: Int, read (c !! 5) :: Double)) filtered
    let grouped = groupYear pairs
    json [object ["year" .= y, "emissions" .= v] | (y,v) <- grouped]

  ------------------------------------------------
  -- TOP 10 COUNTRIES
  ------------------------------------------------
  get "/top10-loss" $ do
    rows <- liftIO loadRows
    let pairs =
          map (\r ->
            let c = splitComma r
            in (c !! 1, read (c !! 3) :: Double)) rows
    let grouped =
          map (\g -> (fst (head g), sumVals (map snd g)))
              (groupBy (\a b -> fst a == fst b) (sortOn fst pairs))
    let top10 = take 10 (reverse (sortOn snd grouped))
    json [object ["country" .= c, "loss" .= v] | (c,v) <- top10]