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
      -- 優勝
      -- この辺のコードはHaskellやめちまえって感じ
      let tmp0 = map (text) tl
      let tmp = map(T.unpack) tmp0
      let tmpPerf = intercalate "\n" tmp

      res<-generateBotTweet tmpPerf
      tweet $ T.pack res
      return ()