{-# LANGUAGE OverloadedStrings #-}
module Deps where

import           System.Process.Typed           ( runProcess_ )

sayHello :: IO ()
sayHello = runProcess_ "hello"
