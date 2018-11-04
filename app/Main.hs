{-# LANGUAGE OverloadedStrings #-}

module Main where

    import Lib
    import qualified Data.Text    as T
    import qualified Data.Text.IO as T
    import qualified Data.List    as D 
    
    main :: IO ()
    main = do
      botGetFollower
      myTweet <- ykbrGetTweet
      -- 取得に成功すれば tl(型は[Tweet]) が出るはず
      case myTweet of
        Left err -> error err
        Right tl -> botTweet tl

    botTweet::[Tweet] -> IO()
    botTweet tl = do
      -- 優勝
      -- この辺のコードはHaskellやめちまえって感じ
      let tmp0 = map(T.unpack) $ map (text) tl
      let tmp = filter((/="@") . take 1) $ filter((/="RT") . take 2) tmp0
      let tmpPerf = D.intercalate "\n" tmp

      res<-generateBotTweet tmpPerf
      tweet $ T.pack res
      return ()