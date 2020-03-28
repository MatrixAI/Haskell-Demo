{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Library where

import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Logger          as L
import           Control.Monad.Logger           ( LoggingT
                                                , MonadLogger
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , ask
                                                , runReaderT
                                                )
import qualified Data.Text                     as T

--------------------------------
-- Library Style 1: mtl class --
--------------------------------
-- Main entry point via MonadLogger
-- Expects caller of the library to supply logging capability via MonadLogger
preFoo :: (MonadLogger m) => String -> m String
preFoo s = $(L.logInfo) "Adding foo as prefix" >> return ("Foo " ++ s)

-- Alternative entry point via IO with default logging capability
preFooIO :: String -> IO String
preFooIO = L.runStderrLoggingT . preFoo

----------------------------------------------
-- Library Style 2: monad transformer stack --
----------------------------------------------
-- Library has its own monadic context with a fully specified stack transformer.
-- Logging capability is present in the stack as LoggingT
newtype Bar a = Bar {unBar :: ReaderT String (LoggingT IO) a} deriving (Functor, Applicative, Monad, MonadReader String, MonadLogger)

-- Library function returns operations wrapped in its monadic context
preBar :: String -> Bar String
preBar s = do
  prefix <- ask
  $(L.logInfo) $ T.pack $ "Adding " ++ prefix ++ " as prefix"
  return $ prefix ++ " " ++ s

-- Main entry point with configurable logging action
runBarWithLogger :: (LoggingT IO a -> IO a) -> Bar a -> IO a
runBarWithLogger runLog bar = runLog $ runReaderT (unBar bar) "Bar"

-- Alternative entry point with default logging action
runBar :: Bar a -> IO a
runBar = runBarWithLogger runStderrLoggingT

------------------------------------------------------------------
-- Library Style 3: mixed monad transformer stack and mtl class --
------------------------------------------------------------------
-- Library has its own monadic context (ReaderT String here),
-- and also allows caller to supply further contexts (capabilities).
-- Non fully specified stack transformer. LoggingT and IO not present in the stack
newtype FooBarT m a = FooBarT {unFooBarT :: ReaderT String m a} deriving (Functor, Applicative, Monad, MonadReader String, MonadLogger)

-- Alternative/convenient type with fully specified transformer stack
type FooBarIO = FooBarT (LoggingT IO)

-- Library function returns operation wrapped in both its own monadic context
-- and that suppied by caller
preFooBar :: (MonadLogger m) => String -> FooBarT m String
preFooBar s = do
  prefix <- ask
  $(L.logInfo) $ T.pack $ "Adding " ++ prefix ++ " as prefix"
  return $ prefix ++ " " ++ s

-- Main entry point mtl class
-- Smart deconstructor expects caller to provide logging capability
runFooBarT :: (MonadLogger m) => FooBarT m a -> m a
runFooBarT fb = runReaderT (unFooBarT fb) "FooBar"

-- Alternative entry point via IO with default logging capability
runFooBarIO :: FooBarIO a -> IO a
runFooBarIO = runStderrLoggingT . runFooBarT
