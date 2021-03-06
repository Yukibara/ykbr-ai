{-# LANGUAGE OverloadedStrings #-}

module Config
    ( TwitterAccessTokens (..)
    , readOAuth
    , readAccessTokens
    ) where

import Data.ByteString hiding (readFile)
import Web.Authenticate.OAuth (OAuth ())

data TwitterAccessTokens = TwitterAccessTokens
    { accessToken       :: ByteString
    , accessTokenSecret :: ByteString
    } deriving (Show, Read)

readOAuth :: IO OAuth
readOAuth = do
  oauth <- read <$> readFile "resource/twitter_oauth"
  return oauth
  
readAccessTokens :: IO TwitterAccessTokens
readAccessTokens = do
  accessTokens <- read <$> readFile "resource/twitter_access_tokens"
  return accessTokens