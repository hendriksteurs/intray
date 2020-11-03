{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.Server.Looper.DB
  ( looperDB,
  )
where

import Database.Persist.Sqlite (SqlPersistT, runSqlPool)
import Import
import Intray.Server.Looper.Import

looperDB :: SqlPersistT IO b -> Looper b
looperDB query = do
  pool <- asks looperEnvConnectionPool
  liftIO $ runSqlPool query pool
