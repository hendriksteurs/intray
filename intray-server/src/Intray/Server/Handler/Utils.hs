{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Server.Handler.Utils
  ( runDB,
    deleteAccountFully,
  )
where

import Database.Persist
import Database.Persist.Sqlite
import Import
import Intray.API
import Intray.Server.Types
import Servant

runDB :: (MonadReader IntrayServerEnv m, MonadIO m) => SqlPersistT IO b -> m b
runDB query = do
  pool <- asks envConnectionPool
  liftIO $ runSqlPool query pool

deleteAccountFully :: AccountUUID -> IntrayHandler ()
deleteAccountFully uuid = do
  mEnt <- runDB $ getBy $ UniqueUserIdentifier uuid
  case mEnt of
    Nothing -> throwError err404 {errBody = "User not found."}
    Just (Entity uid _) ->
      runDB $ do
        deleteWhere [IntrayItemUserId ==. uuid]
        delete uid
