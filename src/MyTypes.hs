module MyTypes where
import Data.Time
import Database.SQLite.Simple.FromRow

--type Day = utctDay <$> getCurrentTime

data Tool = Tool
    { toolId :: Int
    , name :: String
    , description :: String
    , lastReturned :: Day
    , timesBorrowed :: Int
    }

data User = User
    { userId :: Int
    , userName :: String
    }

instance FromRow User where
    fromRow = User <$> field
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
                        , userName user]


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