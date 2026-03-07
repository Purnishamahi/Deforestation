import Web.Scotty
import qualified Data.ByteString.Lazy as BL

main = scotty 3000 $ do
  get "/data" $ do
    csv <- liftIO $ BL.readFile "Final.csv"
    raw csv