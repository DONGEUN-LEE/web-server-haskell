module Env where

import System.Environment (getEnv)

getConnStr :: IO String
getConnStr = getEnv "DATABASE_URL"

getSecret :: IO String
getSecret = getEnv "SECRET_KEY"