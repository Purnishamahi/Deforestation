{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import qualified Data.ByteString.Lazy as BL
import Control.Monad.IO.Class (liftIO)
import Data.List
import qualified Data.Text.Lazy as TL

-- split CSV line by comma
splitComma :: String -> [String]
splitComma [] = [""]
splitComma (',' : xs) = "" : splitComma xs
splitComma (x : xs) =
  let (y:ys) = splitComma xs
  in (x:y) : ys

-- recursive fold aggregation
sumLoss :: [Double] -> Double
sumLoss = foldl (+) 0

-- read dataset
loadRows :: IO [String]
loadRows = do
  csv <- readFile "Final.csv"
  return (drop 1 (lines csv))

main :: IO ()
main = scotty 3000 $ do

  -- API root
  get "/" $ do
    text "Deforestation API running"

  -- return dataset
  get "/data" $ do
    csv <- liftIO $ BL.readFile "Final.csv"
    setHeader "Content-Type" "text/csv"
    raw csv

  -- GLOBAL TREE COVER LOSS
  get "/global-loss" $ do
    rows <- liftIO loadRows

    let losses =
          map (\r -> read (splitComma r !! 3) :: Double) rows

    let totalLoss = sumLoss losses

    text (TL.pack (show (round totalLoss :: Integer)))

  -- GLOBAL FIRE LOSS
  get "/global-fire-loss" $ do
    rows <- liftIO loadRows

    let losses =
          map (\r -> read (splitComma r !! 4) :: Double) rows

    let totalLoss = sumLoss losses

    text (TL.pack (show (round totalLoss :: Integer)))

  -- GLOBAL CO2 EMISSIONS
  get "/global-emissions" $ do
    rows <- liftIO loadRows

    let emissions =
          map (\r -> read (splitComma r !! 5) :: Double) rows

    let total = sumLoss emissions

    text (TL.pack (show (round total :: Integer)))

  -- TOP 10 COUNTRIES BY LOSS
  get "/top10-loss" $ do
    rows <- liftIO loadRows

    let pairs =
          map (\r ->
                let cols = splitComma r
                in (cols !! 1, read (cols !! 5) :: Double)
              ) rows

    let grouped =
          map (\g -> (fst (head g), sumLoss (map snd g)))
          (groupBy (\a b -> fst a == fst b) (sortOn fst pairs))

    let top10 =
          take 10 (reverse (sortOn snd grouped))

    text (TL.pack (show top10))