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

serveAdminGetAccount :: AuthCookie -> Username -> IntrayHandler AccountInfo
serveAdminGetAccount AuthCookie {..} username = do
  mUserEntity <- runDb $ getBy $ UniqueUsername username
  case mUserEntity of
    Nothing -> throwError err404 {errBody = "User not found."}
    Just (Entity _ user) -> getAccountInfoForUser user
