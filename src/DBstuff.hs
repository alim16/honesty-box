module DBstuff where


import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow --probably removable, since it's used mytypes.hs
import Data.Time

import MyTypes

runDBstuff :: IO ()
runDBstuff = print $ show $ User 1 "someName"

myDB :: String
myDB = "../tools.db"


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
        print "user added"

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

