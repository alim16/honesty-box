{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Internal

-- * api

type ItemApi =
  "item" :> Get '[JSON] Item :<|>
  "item" :> ReqBody '[JSON] Item :> Post '[JSON] String
  -- "item" :> Capture "itemId" Integer :> Get '[JSON] Item

itemApi :: Proxy ItemApi
itemApi = Proxy


--post example that worked (json was encoded Item)
-- curl -d '{"itemId":1,"itemText":"some text"}' -H "Content-Type: application/json" -X POST http://localhost:3000/item

-- * app
run :: IO ()
run = do
  let port = 4000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve itemApi server

server :: Server ItemApi
server =
  -- getItems :<|>
  -- getItemById
  getItems :<|> postItem

getItems :: Handler Item
getItems = do
  text <- liftIO readItemFromFile
  case text of
    Just x -> return $ x
    Nothing -> return $ Item 0 "no value"

postItem :: Item -> Handler String
postItem item = do
  liftIO $ BL.writeFile "/tmp/foo.txt" $ encode item
  return $ show (encode item)

-- getItemById :: Integer -> Handler (IO Item)
-- getItemById = \ case
--   0 -> return exampleItem
--   _ -> throwError err404

readItemFromFile :: IO (Maybe Item)
readItemFromFile = do ---"{\"itemId\":1,\"itemText\":\"some monday text\"}"
                text <- readFile "/tmp/foo.txt"
                return $ (decode (BL.pack $ map c2w text) :: Maybe Item) 

ex1 :: Item
ex1 = Item {
  itemId = 1
  , itemText = "some text"
}

-- * item

data Item
  = Item {
    itemId :: Integer,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item