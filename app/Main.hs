{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import qualified Data.ByteString.Lazy as BL
import Control.Monad.IO.Class (liftIO)
import Data.List (sortOn, groupBy)
import qualified Data.Text.Lazy as TL
import Network.Wai (queryString)
import Data.Text.Encoding (decodeUtf8)
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

-- read country from query string
getCountry :: ActionM String
getCountry = do
  req <- request
  let qs = queryString req
  let val = lookup "country" qs
  case val of
    Just (Just v) -> return (TL.unpack (TL.fromStrict (decodeUtf8 v)))
    _             -> return ""

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
    let pairs = map (\r ->
            let c = splitComma r
            in (read (c !! 2), read (c !! 3))) rows
    let grouped = groupYear pairs
    json [object ["year" .= y, "loss" .= v] | (y,v) <- grouped]

  ------------------------------------------------
  -- COUNTRY LOSS
  ------------------------------------------------
  get "/country-loss" $ do
    country <- getCountry
    rows <- liftIO loadRows
    let filtered = filter (\r -> splitComma r !! 1 == country) rows

    let pairs = map (\r ->
            let c = splitComma r
            in (read (c !! 2), read (c !! 3))) filtered

    let grouped = groupYear pairs
    json [object ["year" .= y, "loss" .= v] | (y,v) <- grouped]

  ------------------------------------------------
  -- COUNTRY FIRE
  ------------------------------------------------
  get "/country-fire" $ do
    country <- getCountry
    rows <- liftIO loadRows
    let filtered = filter (\r -> splitComma r !! 1 == country) rows

    let pairs = map (\r ->
            let c = splitComma r
            in (read (c !! 2), read (c !! 4))) filtered

    let grouped = groupYear pairs
    json [object ["year" .= y, "fire" .= v] | (y,v) <- grouped]

  ------------------------------------------------
  -- COUNTRY EMISSIONS
  ------------------------------------------------
  get "/country-emissions" $ do
    country <- getCountry
    rows <- liftIO loadRows
    let filtered = filter (\r -> splitComma r !! 1 == country) rows

    let pairs = map (\r ->
            let c = splitComma r
            in (read (c !! 2), read (c !! 5))) filtered

    let grouped = groupYear pairs
    json [object ["year" .= y, "emissions" .= v] | (y,v) <- grouped]