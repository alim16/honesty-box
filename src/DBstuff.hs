{-# LANGUAGE OverloadedStrings #-}

module DBstuff where


import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow --probably removable, since it's used mytypes.hs
import Data.Time
import Turtle
import GHC.Generics
import Data.Aeson
import Data.Pool (Pool, withResource)
import Data.ByteString (ByteString, unpack, pack)
import Data.Char (chr,ord)

import qualified Data.ByteString as BS

import MyTypes



runDBstuff :: IO ()
runDBstuff = do
    dbInit
    return ()
-- runDBstuff = printUsers --print $ show $ User 1 "someName"

myDB :: String
myDB = "../test2.db"

dbInit :: IO ExitCode
dbInit = proc "sqlite3" [fromString(myDB), dbInitScript] empty


withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
    conn <- open dbName
    action conn
    close conn

addUser :: String -> IO ()
addUser username = withConn myDB $
    \conn -> do
        execute conn "INSERT INTO users (username) values (?)"
            (Only username)
        print "retrieving user"

getUser :: ByteString -> ByteString -> Pool Connection -> IO (Maybe AuthenticatedUser)
getUser name pass conn = do
       user <- withResource conn $
            \conn -> do
                returnedUsers <- liftIO $ query conn "SELECT * from users WHERE lastName = (?)"
                    (Only (bytesToString name)) :: IO [User]
                return (head returnedUsers)
       return (Just (AUser (userId user) 1)) --change to return nothing if user not found or password incorrect
 
bytesToString :: ByteString -> String
bytesToString bytes = map (chr . fromEnum) $ BS.unpack bytes


checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn myDB $
    \conn -> do
        execute conn
            "INSERT INTO checkedout (user_id, user_tool) VALUES (?,?)"
            (userId, toolId)

printUsers :: IO ()
printUsers = withConn myDB $
        \conn -> do
            resp <- query_ conn "SELECT * FROM users;" :: IO [User]
            mapM_ print resp


dbInitScript :: Text
dbInitScript = 
    "DROP TABLE IF EXISTS checkedout;\
    \DROP TABLE IF EXISTS tools;\
    \DROP TABLE IF EXISTS users;\
    \CREATE TABLE users (id INTEGER PRIMARY KEY, firstName TEXT, lastName TEXT, password TEXT, email Text NOT NULL UNIQUE,roleId INTEGER);\
    \CREATE TABLE tools (id INTEGER PRIMARY KEY,name TEXT,description TEXT,lastReturned TEXT,timesBorrowed INTEGER);\
    \CREATE TABLE checkedout (user_id INTEGER,tool_id INTEGER);\
    \INSERT INTO users (firstName, lastName, password, email , roleId) VALUES ('will', 'kurt', 'secret','kurt@test.com', 1);\
    \INSERT INTO tools (name,description,lastReturned,timesBorrowed)\
    \VALUES ('hammer','hits stuff','2017-01-01',0);\
    \INSERT INTO tools (name,description,lastReturned,timesBorrowed)\
    \VALUES ('saw','cuts stuff','2017-01-01',0);\
    \SELECT * from users WHERE lastName = 'kurt';"