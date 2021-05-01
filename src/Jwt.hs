{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- just for sweet and short examples
{-# LANGUAGE NoMonomorphismRestriction #-}

module Jwt where

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Default
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics
import Web.Libjwt
import Prelude hiding (exp)
import Data.ByteString.Char8 (pack)
import Env

data UserClaims = UserClaims
  { email :: String,
    isRoot :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance ToPrivateClaims UserClaims

hmac256 :: String -> Algorithm Secret
hmac256 secret = do
  HMAC256 $ MkSecret (pack secret)

-- mkPayload UserClaims {..} currentTime =
--   let now = fromUTC currentTime
--   in def
--         { iss = Iss (Just "myApp"),
--           aud = Aud ["https://myApp.com"],
--           iat = Iat (Just now),
--           exp = Exp (Just $ now `plusSeconds` 300),
--           privateClaims =
--             toPrivateClaims
--               ( #email ->> email,
--                 #is_root ->> isRoot,
--                 #created ->> createdAt
--               )
--         }
mkPayload email = jwtPayload
  (withIssuer "myApp" <> withRecipient "https://myApp.com" <> setTtl 300)
  UserClaims { email     = email
             , isRoot    = False
             }

getJwt :: String -> IO ByteString -- or any other MonadTime instance
getJwt email = do
  secret <- getSecret
  getToken . sign (hmac256 secret) <$> mkPayload email
