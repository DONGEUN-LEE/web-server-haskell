{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Model where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT, runStderrLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import GHC.Generics ( Generic )
import Data.ByteString.UTF8 as BSU
import Data.Int
import Data.Text (Text)
import Data.Time
import Data.Pool
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Yesod
import Env

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Plans
  siteId String
  stageId String
  operId String
  resourceId String
  productId String
  planQty Double
  startTime UTCTime
  endTime UTCTime
Users
  email String
  password String
  deriving Show
|]

data LoginRes = LoginRes
  { token :: String,
    message :: String
  }
  deriving (Show, Generic)

-- this is the repeated code that can be factored out
inBackend :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
inBackend action = do
  connStr <- getConnStr
  runStderrLoggingT $
    withPostgresqlPool (BSU.fromString connStr) 10 $ \pool -> liftIO $
      flip runSqlPersistMPool pool $ do
    runMigration migrateAll
    action

appConnPool :: IO (Pool SqlBackend)
appConnPool = do
  connStr <- getConnStr
  runStderrLoggingT $ createPostgresqlPool (BSU.fromString connStr) 10
