{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( Tweet(..)
    ,tweet
    ) where

import Data.Text
import Data.Text.Encoding
import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Config

newtype Tweet = Tweet { text :: Text
                       } deriving (Show, Generic)

instance FromJSON Tweet
instance ToJSON Tweet

botName = "ykbr-ai"

newCredential' :: TwitterAccessTokens -> Credential
newCredential' accessTokens =
    let accessTokenValue       = accessToken accessTokens
        accessTokenSecretValue = accessTokenSecret accessTokens
    in newCredential accessTokenValue accessTokenSecretValue



tweet :: Text -> IO ()
tweet tw = do
    botOAuth <- readOAuth
    accessTokens <- readAccessTokens
    let botCredential = newCredential' accessTokens
    req     <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
    manager <- newManager tlsManagerSettings
    let postReq = urlEncodedBody [("status", encodeUtf8 tw)] req
    res <- do
        signedReq <- signOAuth botOAuth botCredential postReq
        httpLbs signedReq manager
    return ()