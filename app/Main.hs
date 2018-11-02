{-# LANGUAGE OverloadedStrings #-}

module Main where

    import Lib
    import qualified Data.Text.IO as T
    
    main :: IO ()
    main = do
      yT <- ykbrTweet
      case yT of
        Left err -> error err
        Right tl -> mapM_ (T.putStrLn . text) $ tl