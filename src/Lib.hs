{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( Tweet(..)
    ,tweet
    ,ykbrTweet
    ,generateBotTweet
    ) where

import Prelude
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding
import Data.List (intercalate)
import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Config
import Text.MeCab

newtype Tweet = Tweet { text :: T.Text
                       } deriving (Show, Generic)

instance FromJSON Tweet
instance ToJSON Tweet

botName = "ykbr__ai"
ykbrID = "790512282753048577"

newCredential' :: TwitterAccessTokens -> Credential
newCredential' accessTokens =
    let accessTokenValue       = accessToken accessTokens
        accessTokenSecretValue = accessTokenSecret accessTokens
    in newCredential accessTokenValue accessTokenSecretValue

tweet :: T.Text -> IO ()
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

ykbrTweet :: IO (Either String [Tweet])
ykbrTweet = do 
    -- 認証準備とかをします
    botOAuth <- readOAuth
    accessTokens <- readAccessTokens
    let botCredential = newCredential' accessTokens

    -- 取得
    result <- do
        req <-
            parseRequest $ "https://api.twitter.com/1.1/statuses/user_timeline.json?user_id="
            ++ ykbrID ++ "&count=200"
        signedReq <- signOAuth botOAuth botCredential req
        manager   <- newManager tlsManagerSettings
        httpLbs signedReq manager
    return $ eitherDecode $ responseBody result

generateBotTweet::String -> IO()
generateBotTweet tweet = do
    mecab <- new2 ""
    cutLine <- mapM (parseToNodes mecab) (lines tweet) 
    let sliceLine = map (filter(not . null) . map nodeSurface) cutLine
    let seedWord = intercalate ["\n"] sliceLine



    return ()