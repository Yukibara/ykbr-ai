{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( Tweet(..)
    ,tweet
    ,ykbrGetTweet
    ,generateBotTweet
    ) where

import Prelude hiding(Word)
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
import System.Random (randomRIO)

newtype Tweet = Tweet { getText :: T.Text } deriving (Show, Generic)
data Word = Begin|Middle String|End deriving (Eq,Show, Generic)

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

ykbrGetTweet :: IO (Either String [Tweet])
ykbrGetTweet = do 
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

-- TODO:関数を実装する
-- makeTable    n文字ごとに切り出してテーブルを作る
-- startWord どこから切り出すか決める
-- beginWord    文頭(Nullとかで表現)で始まる行をランダムで選ぶ
-- chainWord    テーブルから連鎖するワードを選ぶ ここで連鎖させてもいいかも
-- hogehogeFunc リストを連結させていい感じにする
generateBotTweet::String -> IO String
generateBotTweet tweet = do
    mecab <- new2 ""
    cuutLine <- mapM(parseToNodes mecab) (lines tweet) 
    let sliceLine = map (filter(not . null) . map nodeSurface) cuutLine
    let seedWord = intercalate ["\n"] sliceLine

    -- 3文字ごとに切り出してテーブルを作る
    let kouho = makeTable 3 (startWord seedWord)

    tmp <- beginWord kouho
    case tmp of
        Nothing -> return ""
        Just ra -> chainWord kouho (tail ra) (fromWord ra)

chainWord::[[Word]] -> [Word] -> String -> IO String
chainWord table prefix chain = do
    next <- nextWord table prefix
    if last prefix == End
    then return chain
    else case next of 
        Nothing -> return chain
        Just ws -> chainWord table (tail ws)(chain ++ maybe "" fromWord'(last <$> next))

fromWord::[Word]->String
fromWord = concatMap fromWord'

fromWord'::Word -> String
fromWord' (Middle x) = x
fromWord' _ =""

nextWord::[[Word]]->[Word]->IO(Maybe [Word])
nextWord table prefix = randomSel(filter f table)
    where
        f xs=init xs==prefix

beginWord::[[Word]]->IO(Maybe [Word])
beginWord table = randomSel(filter f table)
    where
        f(x:xs) = x == Begin

randomSel::[a] -> IO(Maybe a)
randomSel xs = return <$> randomOne xs
    where
        randomOne xs = (xs !!) <$> randomRIO(0,length xs-1)

startWord::[String]->[Word]
startWord xs = Begin:init (convert xs)
    where
        convert::[String] -> [Word]
        convert[]=[]
        convert(x:xs)=
            if x=="\n"
            then End:Begin:convert xs
            else Middle x:convert xs


makeTable::Int -> [Word] -> [[Word]]
makeTable _ [] = []
makeTable n abc@(x:xs)
    | length abc < n=[]
    | End `elem` init part = makeTable n xs
    | otherwise = part : makeTable n xs
    where
        part = take n abc


    -- エラーが15個くらい出ています
    -- 終了