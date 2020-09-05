{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Demo where

import qualified Control.Exception.Safe        as E
import           Control.Exception.Safe         ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Control.Monad.Logger          as L
import           Control.Monad.Logger           ( LoggingT
                                                , MonadLogger
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , ask
                                                , runReaderT
                                                )
import           Control.Monad.Trans.Class      ( MonadTrans
                                                , lift
                                                )
import           Data.Default.Class             ( Default
                                                , def
                                                )
import           Data.IP                        ( IP )
import           Data.String                    ( fromString )
import qualified Network.HTTP.Types            as HTTP
import qualified Network.HTTP.Types.Header     as HTTPHeaders
import           Network.Socket                 ( PortNumber )
import           Network.Wai                    ( Application )
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp
import           System.Environment             ( lookupEnv )


data DemoEnv = DemoEnv { demoEnvHost :: IP,
                         demoEnvPort :: PortNumber
                       } deriving (Show)

-- this is the default demoenv
instance Default DemoEnv where
  def = DemoEnv "127.0.0.1" 55555

newtype DemoT m a = DemoT
  { runDemoT :: ReaderT DemoEnv (LoggingT m) a }
  deriving (
    Functor,
    Applicative,
    Monad,
    MonadIO,
    MonadThrow,
    MonadCatch,
    MonadLogger,
    MonadReader DemoEnv
    )

type Demo = DemoT IO

runDemo :: DemoEnv -> Demo a -> IO a
runDemo env demo = L.runStderrLoggingT $ runReaderT (runDemoT demo) env

warpApp :: Application
warpApp req respond = E.bracket_
  (L.runStderrLoggingT ($(L.logInfo) "Try IO Block"))
  (L.runStderrLoggingT ($(L.logInfo) "Clean IO Block"))
  (respond $ Wai.responseLBS HTTP.status200
                             [(HTTPHeaders.hContentType, "text/plain")]
                             "Hello from Demo!\n"
  )

demoApp :: Demo ()
demoApp = do
  $(L.logInfo) "Starting Demo Server"
  DemoEnv ip port <- ask
  let settings =
        Warp.setHost (fromString $ show ip)
          $ Warp.setPort (fromIntegral port)
          $ Warp.defaultSettings
  $(L.logInfo) $ fromString $ "Running on " ++ show ip ++ ":" ++ show port
  liftIO $ Warp.runSettings settings warpApp
  $(L.logInfo) "Terminated Demo Server"

runDemoApp :: IO ()
runDemoApp = do
  demoHost <- lookupEnv "DEMO_HOST"
  demoPort <- lookupEnv "DEMO_PORT"
  let defaultEnv = def :: DemoEnv
  defaultEnv <- return $ maybe
    defaultEnv
    (\host -> defaultEnv { demoEnvHost = read host })
    demoHost
  defaultEnv <- return $ maybe
    defaultEnv
    (\port -> defaultEnv { demoEnvPort = read port })
    demoPort
  runDemo defaultEnv demoApp
