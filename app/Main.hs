{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Time
import Control.Monad.Trans.Reader (ReaderT)
import Crypto.BCrypt
import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Parser (json)
import Data.ByteString.Char8 (pack, unpack)
import Data.Conduit (($=))
import qualified Data.Conduit.List as CL
import Data.Maybe (fromMaybe, maybeToList)
import Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E
import Env
import Jwt
import Model
import Yesod

data App = App

instance Yesod App

instance YesodPersist App where
  type YesodPersistBackend App = E.SqlBackend
  runDB action = do
    pool <- liftIO appConnPool
    E.runSqlPool action pool

instance YesodPersistRunner App where
  getDBRunner = do
    pool <- liftIO appConnPool
    defaultGetDBRunner $ const pool

instance ToJSON (Entity Plans) where
  toJSON (Entity pid p) =
    object
      [ "siteId" .= plansSiteId p,
        "stageId" .= plansStageId p,
        "operId" .= plansOperId p,
        "resourceId" .= plansResourceId p,
        "productId" .= plansProductId p,
        "planQty" .= plansPlanQty p,
        "startTime" .= plansStartTime p,
        "endTime" .= plansEndTime p
      ]

instance ToJSON (Entity Users) where
  toJSON (Entity uid u) =
    object
      [ "email" .= usersEmail u,
        "password" .= usersPassword u
      ]

instance ToJSON Users where
  toJSON u =
    object
      [ "email" .= usersEmail u,
        "password" .= usersPassword u
      ]

instance FromJSON Users where
  parseJSON (Object o) =
    Users
      <$> o .: "email"
      <*> o .: "password"

instance ToJSON LoginRes where
  toJSON (LoginRes token message) =
    object ["token" .= token, "message" .= message]

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET
/api/plan PlanR GET
/api/login LoginR POST
|]

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

getPlanR :: Handler Value
getPlanR = do
  plans <- runDB $ selectList [] [] :: Handler [Entity Plans]
  return $ object ["plans" .= plans]

postLoginR :: Handler Value
postLoginR = do
  users <- requireInsecureJsonBody :: Handler Users
  let email = usersEmail users
  let password = usersPassword users
  -- hash <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (pack password)
  -- let hashPass = unpack $ fromMaybe (pack "") hash
  userSrc <- runDB $ selectFirst [UsersEmail ==. email] [] :: Handler (Maybe (Entity Users))
  let userRow = entityVal $ head $ maybeToList userSrc
  let rowPass = usersPassword userRow

  jwt <- liftIO $ getJwt email

  if validatePassword (pack rowPass) (pack password)
    then
      returnJson
        LoginRes
          { token = unpack jwt,
            message = "Success"
          }
    else
      returnJson
        LoginRes
          { token = "",
            message = "Fail"
          }

main :: IO ()
main = do
  loadFile defaultConfig
  warp 3000 App
