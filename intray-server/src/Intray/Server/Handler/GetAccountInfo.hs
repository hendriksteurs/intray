{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.GetAccountInfo
  ( serveGetAccountInfo,
  )
where

import Database.Persist
import Import
import Intray.API
import Intray.Server.AccountInfo
import Intray.Server.Handler.Utils
import Intray.Server.Types
import Servant

serveGetAccountInfo :: AuthCookie -> IntrayHandler AccountInfo
serveGetAccountInfo AuthCookie {..} = do
  mUser <- runDb $ getBy $ UniqueUserIdentifier authCookieUserUUID
  case mUser of
    Nothing -> throwError err404 {errBody = "User not found."}
    Just (Entity _ user) -> getAccountInfoForUser user
