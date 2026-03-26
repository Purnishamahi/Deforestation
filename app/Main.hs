{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import qualified Data.ByteString.Lazy as BL
import Control.Monad.IO.Class (liftIO)
import Data.List (sortOn, groupBy)
import qualified Data.Text.Lazy as TL
import Network.Wai (queryString)
import Data.Text.Encoding (decodeUtf8)
import Data.Aeson (object, (.=))
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger

------------------------------------------------
-- CSV SPLIT FUNCTION
------------------------------------------------

splitComma :: String -> [String]
splitComma [] = [""]
splitComma (',' : xs) = "" : splitComma xs
splitComma (x : xs) =
  let (y:ys) = splitComma xs
  in (x:y) : ys

------------------------------------------------
-- RECURSIVE FOLD (AGGREGATION)
------------------------------------------------

sumVals :: [Double] -> Double
sumVals = foldl (+) 0

------------------------------------------------
-- READ DATASET
------------------------------------------------

loadRows :: IO [String]
loadRows = do
  csv <- readFile "Final.csv"
  return (drop 1 (lines csv))

------------------------------------------------
-- GROUP BY YEAR
------------------------------------------------

groupYear :: [(Int, Double)] -> [(Int, Double)]
groupYear pairs =
  map (\g -> (fst (head g), sumVals (map snd g)))
  (groupBy (\a b -> fst a == fst b) (sortOn fst pairs))

------------------------------------------------
-- GET COUNTRY FROM QUERY
------------------------------------------------

getCountry :: ActionM String
getCountry = do
  req <- request
  let qs = queryString req
  let val = lookup "country" qs
  case val of
    Just (Just v) -> return (TL.unpack (TL.fromStrict (decodeUtf8 v)))
    _             -> return ""

------------------------------------------------
-- MAIN SERVER
------------------------------------------------

main :: IO ()
main = scotty 3000 $ do

  middleware logStdoutDev
  middleware $ cors (const $ Just simpleCorsResourcePolicy)

------------------------------------------------
-- HTML PAGES
------------------------------------------------

  get "/" $ do
    setHeader "Content-Type" "text/html"
    file "frontend/home.html"

  get "/home.html" $ do
    setHeader "Content-Type" "text/html"
    file "frontend/home.html"

  get "/globalloss.html" $ do
    setHeader "Content-Type" "text/html"
    file "frontend/globalloss.html"

  get "/globalemission.html" $ do
    setHeader "Content-Type" "text/html"
    file "frontend/globalemission.html"

  get "/globalfireloss.html" $ do
    setHeader "Content-Type" "text/html"
    file "frontend/globalfireloss.html"

  get "/top10.html" $ do
    setHeader "Content-Type" "text/html"
    file "frontend/top10.html"  

  get "/maps.html" $ do
    setHeader "Content-Type" "text/html"
    file "frontend/maps.html"

------------------------------------------------
-- DATA DOWNLOAD
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

  get "/global-fire-yearly" $ do
    rows <- liftIO loadRows
    let pairs = map (\r ->
          let c = splitComma r
          in (read (c !! 2), read (c !! 4))) rows
    let grouped = groupYear pairs
    json [object ["year" .= y, "fire" .= v] | (y,v) <- grouped]

  get "/global-emissions-yearly" $ do
    rows <- liftIO loadRows
    let pairs = map (\r ->
          let c = splitComma r
          in (read (c !! 2), read (c !! 5))) rows
    let grouped = groupYear pairs
    json [object ["year" .= y, "emissions" .= v] | (y,v) <- grouped]

------------------------------------------------
-- COUNTRY FOREST LOSS
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
-- COUNTRY FIRE LOSS
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

------------------------------------------------
-- TOP 10 FOREST LOSS COUNTRIES
------------------------------------------------

  get "/top10-loss" $ do
    rows <- liftIO loadRows

    let pairs = map (\r ->
          let c = splitComma r
          in (c !! 1, read (c !! 3) :: Double)) rows

    let sorted = sortOn fst pairs

    let grouped =
          map (\g -> (fst (head g), sumVals (map snd g)))
          (groupBy (\a b -> fst a == fst b) sorted)

    let top10 = take 10 (reverse (sortOn snd grouped))

    json [object ["country" .= c, "loss" .= v] | (c,v) <- top10]

------------------------------------------------
-- TOP 10 FIRE LOSS COUNTRIES
------------------------------------------------

  get "/top10-fire" $ do
    rows <- liftIO loadRows

    let pairs = map (\r ->
          let c = splitComma r
          in (c !! 1, read (c !! 4) :: Double)) rows

    let sorted = sortOn fst pairs

    let grouped =
          map (\g -> (fst (head g), sumVals (map snd g)))
          (groupBy (\a b -> fst a == fst b) sorted)

    let top10 = take 10 (reverse (sortOn snd grouped))

    json [object ["country" .= c, "fire" .= v] | (c,v) <- top10]

------------------------------------------------
-- TOP 10 EMISSIONS COUNTRIES
------------------------------------------------

  get "/top10-emissions" $ do
    rows <- liftIO loadRows

    let pairs = map (\r ->
          let c = splitComma r
          in (c !! 1, read (c !! 5) :: Double)) rows

    let sorted = sortOn fst pairs

    let grouped =
          map (\g -> (fst (head g), sumVals (map snd g)))
          (groupBy (\a b -> fst a == fst b) sorted)

    let top10 = take 10 (reverse (sortOn snd grouped))

    json [object ["country" .= c, "emissions" .= v] | (c,v) <- top10]

    ------------------------------------------------
-- MAP DATA (TOTAL PER COUNTRY)
------------------------------------------------

  get "/map-data" $ do
    rows <- liftIO loadRows

    let triples =
          map (\r ->
            let c = splitComma r
            in ( c !! 1
               , read (c !! 3) :: Double
               , read (c !! 4) :: Double
               , read (c !! 5) :: Double
               )
          ) rows

    let sorted = sortOn (\(c,_,_,_) -> c) triples

    let grouped =
          map (\g ->
            let (country,_,_,_) = head g
                forestSum = sumVals [f | (_,f,_,_) <- g]
                fireSum   = sumVals [fi | (_,_,fi,_) <- g]
                co2Sum    = sumVals [co | (_,_,_,co) <- g]
            in object
                [ "country" .= country
                , "forest"  .= forestSum
                , "fire"    .= fireSum
                , "co2"     .= co2Sum
                ]
          )
          (groupBy (\(a,_,_,_) (b,_,_,_) -> a == b) sorted)

    json grouped