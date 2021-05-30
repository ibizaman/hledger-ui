{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Main
  ( main,
  )
where

import Api (API, api)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.IO.Class
  ( MonadIO,
    liftIO,
  )
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader
  ( MonadReader,
    ReaderT,
    ask,
    runReaderT,
  )
import Data.Function ((&))
import Data.Pool (Pool)
import qualified Database.Persist.Sql as PSql
import qualified Database.Persist.Sqlite as PSqlite
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
  ( cors,
    corsRequestHeaders,
    simpleCorsResourcePolicy,
  )
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Servant.API ((:<|>) (..))
import qualified Servant.Server as Server
import System.IO
  ( BufferMode (LineBuffering),
    hSetBuffering,
    stdout,
  )
import Types (Settings (..), migrateAll)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Resource (runResourceT)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "Starting on port 8080"
  pool <- runResourceT $ runStderrLoggingT $ PSqlite.createSqlitePool "sqlite.db" 5
  runDB pool $ PSqlite.runMigration migrateAll
  Warp.runSettings settings $ app Options {dbPool = pool}
  where
    settings =
      Warp.defaultSettings
        & Warp.setPort 8080
        & Warp.setHost "!4"
        & Warp.setOnOpen (\sockaddr -> print sockaddr >> return True)
        & Warp.setLogger
          ( \request status fileSize ->
              putStrLn $
                ">> request: "
                  <> show request
                  <> "\n>> status: "
                  <> show status
                  <> "\n>> file size: "
                  <> show fileSize
          )

newtype App a = App
  { runApp :: ReaderT Options IO a
  }
  deriving (Monad, Functor, Applicative, MonadReader Options, MonadIO, MonadThrow)

data Options = Options
  { dbPool :: Pool PSql.SqlBackend
  }

runDB :: MonadUnliftIO m => Pool PSql.SqlBackend -> ReaderT PSql.SqlBackend m a -> m a
runDB = flip PSql.runSqlPool

app :: Options -> Server.Application
app options =
  cors (const $ Just policy) $
    provideOptions api $
      Server.serve api $
        Server.hoistServer api (readerToHandler options) server
  where
    policy =
      simpleCorsResourcePolicy
        { corsRequestHeaders = ["content-type"]
        }

readerToHandler :: Options -> App a -> Server.Handler a
readerToHandler options r = liftIO $ runReaderT (runApp r) options

server :: Server.ServerT API App
server = list :<|> add :<|> remove :<|> setJournalLocation :<|> getJournalLocation
  where
    list = do
      Options {dbPool} <- ask
      liftIO $ runDB dbPool $ PSql.selectList [] []
    add item = do
      Options {dbPool} <- ask
      liftIO $ runDB dbPool $ PSql.insert item
    remove itemId = do
      Options {dbPool} <- ask
      liftIO $ runDB dbPool $ PSql.delete itemId

    setJournalLocation newJournalLocation = do
      Options {dbPool} <- ask
      let settingsId = PSql.toSqlKey 0
      liftIO $ runDB dbPool $ PSql.repsert settingsId Settings {settingsJournalLocation = newJournalLocation}
      return ()

    getJournalLocation = do
      Options {dbPool} <- ask
      let settingsId = PSql.toSqlKey 0
      liftIO $ runDB dbPool $ fmap settingsJournalLocation <$> PSql.get settingsId
