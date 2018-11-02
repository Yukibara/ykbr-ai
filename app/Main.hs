{-# LANGUAGE OverloadedStrings #-}

module Main where

    import Lib
    import qualified Data.Text.IO as T
    
    main :: IO ()
    main = do
      myTweet <- ykbrTweet
      case myTweet of
        Left err -> error err
        Right tl -> generateBotTweet tl
