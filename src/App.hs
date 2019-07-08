{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  DeriveGeneric, TypeOperators #-} --add these to package.yaml

module App where

import Data.Aeson
import GHC.Generics
import Data.Proxy
import System.IO
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Servant as S
import Servant.Client
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Control.Monad.IO.Class (liftIO)
import Data.Map as M --remove later
import Data.ByteString (ByteString)
import DBstuff
--import Data.Pool -- to be used later for dbconnection

port :: Int
port = 3001

-----

data AuthenticatedUser = AUser { auID :: Int
                               , role :: Int --to decide access rights
                               } deriving (Show, Generic) --move to myTypes file

instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

-----

type Login      = ByteString
type Password   = ByteString
type DB         = Map (Login, Password) AuthenticatedUser
type Connection = DB
type Pool a     = a

initConnPool :: IO (Pool Connection)
initConnPool = pure $ fromList [ (("user", "pass"), AUser 1 1)
                               , (("user2", "pass2"), AUser 2 1) ]

authCheck :: Pool Connection
          -> BasicAuthData
          -> IO (AuthResult AuthenticatedUser)
authCheck connPool (BasicAuthData login password) = pure $
    --add call to  a withResource function here, which takes user and pass and returns :: Maybe AuthenticatedUser
    -- that's what the Map.lookup does
    maybe SAS.Indefinite Authenticated $ M.lookup (login, password) connPool

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

instance FromBasicAuthData AuthenticatedUser where
    fromBasicAuthData authData authCheckFunction = authCheckFunction authData


-----
type ProtectedAPI = Auth '[SA.JWT, SA.BasicAuth] AuthenticatedUser :> ("users" :> Get '[JSON] () :<|> 
    "users" :> Capture "i" Int :> Get '[JSON] String)

type Public1 = "public1" :> Get '[JSON] String

type MyAPI = Public1 :<|> ProtectedAPI

-- type TestAPIServer =
--     Auth '[SA.JWT, SA.BasicAuth] AuthenticatedUser :> TestAPI


serverNew :: Server MyAPI
serverNew =
  let publicAPIHandler = do
        liftIO $ printUsers --dbfunction to print users
        return $ concat ["foo", "bar"]

      privateHandler3  (Authenticated user) = first :<|> second
        where
          first :: Handler ()
          first =  liftIO $ hPutStrLn stderr $
            concat ["foo: ", show user, " / "]
          second ::  Int -> Handler String
          second n = return $ concat ["hello ", (show user), show n]
      privateHandler3 _ = throwAll err401
     
  in publicAPIHandler :<|> privateHandler3 -- privateAPIHandler2 :<|> privateAPIHandler1

------
mkApp :: Pool Connection -> IO Application
mkApp connPool = do
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
      authCfg = authCheck connPool
      cfg = jwtCfg :. defaultCookieSettings :. authCfg :. EmptyContext
      api = Proxy :: Proxy MyAPI
  pure $ serveWithContext api cfg serverNew


run :: IO ()
run = do
    connPool <- initConnPool
    let settings =
            setPort port $
            setBeforeMainLoop (hPutStrLn stderr
                            ("listening on port " ++ show port)) $ defaultSettings
    runSettings settings =<< mkApp connPool