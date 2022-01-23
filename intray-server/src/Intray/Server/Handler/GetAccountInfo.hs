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
import Intray.Server.Handler.Stripe
import Intray.Server.Handler.Utils
import Intray.Server.Types
import Servant

serveGetAccountInfo :: AuthCookie -> IntrayHandler AccountInfo
serveGetAccountInfo AuthCookie {..} = do
  admins <- asks envAdmins
  mUser <- runDb $ getBy $ UniqueUserIdentifier authCookieUserUUID
  case mUser of
    Nothing -> throwError err404 {errBody = "User not found."}
    Just (Entity _ User {..}) -> do
      c <- runDb $ count ([IntrayItemUserId ==. authCookieUserUUID] :: [Filter IntrayItem])
      ups <- getUserPaidStatus authCookieUserUUID
      pure
        AccountInfo
          { accountInfoUUID = authCookieUserUUID,
            accountInfoUsername = userUsername,
            accountInfoCreatedTimestamp = userCreatedTimestamp,
            accountInfoLastLogin = userLastLogin,
            accountInfoAdmin = userUsername `elem` admins,
            accountInfoCount = c,
            accountInfoStatus = ups
          }
