{-# LANGUAGE OverloadedStrings #-}

module Main where

    import Lib
    import qualified Data.Text    as T
    import qualified Data.Text.IO as T
    import Data.List (intercalate)
    
    main :: IO ()
    main = do
      myTweet <- ykbrGetTweet
      -- 取得に成功すれば tl(型は[Tweet]) が出るはず
      case myTweet of
        Left err -> error err
        Right tl -> botTweet tl

    botTweet::[Tweet] -> IO()
    botTweet tl = do
      -- ? Couldn't match type ‘Tweet’ with ‘[Char]’
      --   Expected type: [[Char]]
      --   Actual type: [Tweet]
      -- [[Char]]なので実質[String]
      -- 型があれ [Tweet]をどうにかしたい
      -- ソリューション: [Tweet]をどうにかして[String]にする 優勝
      let tmp = T.unpack $ text $ head tl
      -- tlのheadをtextでTextにしてunpackでStringに

      res<-generateBotTweet tmp
      tweet $ T.pack res
      return ()

    -- twToStringL::[Tweet] -> [String]
    -- twToStringL twl = do
