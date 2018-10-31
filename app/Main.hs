{-# LANGUAGE OverloadedStrings #-}

module Main where

    import Lib
    import qualified Data.Text.IO as T
    
    main :: IO ()
    main = do
      tweet "テストですよ、聞こえますか"