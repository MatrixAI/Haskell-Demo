{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Logger          as L
import qualified Data.Text                     as T

import           Library                       as Lib

main :: IO ()
main = do
  putStrLn "Please enter some texts: "
  msg <- getLine
  foo msg
  foo' msg
  bar msg
  bar' msg
  foobar msg
  foobar' msg

-- call preFoo and supply a file based logging capability
foo :: String -> IO ()
foo msg = L.runFileLoggingT "/tmp/demo.log" $ do
  liftIO $ putStrLn ""
  $(L.logInfo) $ T.pack $ "[foo] Got user input: " ++ msg
  out <- preFoo msg
  liftIO $ putStrLn out

-- call preFooIO and the inner logging will be library default stderr based logging
foo' :: String -> IO ()
foo' msg = L.runFileLoggingT "/tmp/demo.log" $ do
  liftIO $ putStrLn ""
  $(L.logInfo) $ T.pack $ "[foo'] Got user input: " ++ msg
  out <- liftIO $ preFooIO msg
  liftIO $ putStrLn out

-- call runBarWith and supply file based logging action of caller
bar :: String -> IO ()
bar msg = L.runFileLoggingT "/tmp/demo.log" $ do
  liftIO $ putStrLn ""
  $(L.logInfo) $ T.pack $ "[bar] Got user input: " ++ msg
  out <- liftIO $ runBar $ preBar msg
  liftIO $ putStrLn out

-- call runBarWithLogger and the inner logging is fixed to stderr logging again
bar' :: String -> IO ()
bar' msg = L.runFileLoggingT "/tmp/demo.log" $ do
  liftIO $ putStrLn ""
  $(L.logInfo) $ T.pack $ "[bar'] Got user input: " ++ msg
  -- get the logging action from caller's logging context
  logAction <- L.askLoggerIO
  out       <- liftIO $ runBarWithLogger (`runLoggingT` logAction) $ preBar msg
  liftIO $ putStrLn out

-- run runFooBarT and supply a file based logging capability
foobar :: String -> IO ()
foobar msg = L.runFileLoggingT "/tmp/demo.log" $ do
  liftIO $ putStrLn ""
  $(L.logInfo) $ T.pack $ "[foobar] Got user input: " ++ msg
  out <- runFooBarT $ preFooBar msg
  liftIO $ putStrLn out

-- run runFooBarIO and the inner logging will be library default stderr based logging
foobar' :: String -> IO ()
foobar' msg = L.runFileLoggingT "/tmp/demo.log" $ do
  liftIO $ putStrLn ""
  $(L.logInfo) $ T.pack $ "[foobar'] Got user input: " ++ msg
  out <- liftIO $ runFooBarIO $ preFooBar msg
  liftIO $ putStrLn out
