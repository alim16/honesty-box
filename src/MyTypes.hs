{-# LANGUAGE DeriveGeneric, OverloadedStrings #-} 

module MyTypes where

import Data.Aeson
import GHC.Generics
import Data.Time
import Database.SQLite.Simple.FromRow
import Data.ByteString (ByteString)

import Servant.Auth.Server (FromJWT, ToJWT)

--type Day = utctDay <$> getCurrentTime

instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

data AuthenticatedUser = AUser { auID :: Int
                               , roleID :: Int --to decide access rights, 1 is admin, 2 user
                               } deriving (Show, Generic) --move to myTypes file



data Tool = Tool
    { toolId :: Int
    , name :: String
    , description :: String
    , lastReturned :: Day
    , timesBorrowed :: Int
    }

data User = User
    { userId :: Int
    , firstName :: String
    , lastName :: String
    , password :: String
    , email :: String
    , roleId :: Int
    }

instance FromRow User where
    fromRow = User <$> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field

instance FromRow Tool where
    fromRow = Tool <$> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field


instance Show User where
    show user = mconcat [ show $ userId user
                        , "). "
                        , concat [firstName user," ",lastName user]]


instance Show Tool where
    show tool = mconcat [ show $ toolId tool
                        , ".) "
                        , name tool
                        , "\n description: "
                        , description tool
                        , "\n last returned: "
                        , show $ lastReturned tool
                        , "\n times borrowed: "
                        , show $ timesBorrowed tool
                        , "\n"]