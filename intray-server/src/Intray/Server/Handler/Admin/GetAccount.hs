{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.Admin.GetAccount
  ( serveAdminGetAccount,
  )
where

import Database.Persist
import Import
import Intray.API
import Intray.Server.AccountInfo
import Intray.Server.Handler.Utils
import Intray.Server.Types
import Servant

serveAdminGetAccount :: AuthCookie -> AccountUUID -> IntrayHandler AccountInfo
serveAdminGetAccount AuthCookie {..} accountUuid = do
  mUserEntity <- runDb $ getBy $ UniqueUserIdentifier accountUuid
  case mUserEntity of
    Nothing -> throwError err404 {errBody = "User not found."}
    Just (Entity _ user) -> getAccountInfoForUser user
