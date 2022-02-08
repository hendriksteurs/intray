{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.Admin.GetAccount
  ( serveAdminGetAccount,
    getAccountInfoForUser,
  )
where

import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Stripe
import Intray.Server.Handler.Utils
import Intray.Server.Types
import Servant

serveAdminGetAccount :: AuthCookie -> AccountUUID -> IntrayHandler AccountInfo
serveAdminGetAccount AuthCookie {..} accountUuid = do
  mUserEntity <- runDb $ getBy $ UniqueUserIdentifier accountUuid
  case mUserEntity of
    Nothing -> throwError err404 {errBody = "User not found."}
    Just (Entity _ user) -> getAccountInfoForUser user

getAccountInfoForUser :: User -> IntrayHandler AccountInfo
getAccountInfoForUser User {..} = do
  admins <- asks envAdmins
  c <- runDb $ count ([IntrayItemUserId ==. userIdentifier] :: [Filter IntrayItem])
  ups <- getUserPaidStatus userIdentifier
  pure
    AccountInfo
      { accountInfoUUID = userIdentifier,
        accountInfoUsername = userUsername,
        accountInfoCreatedTimestamp = userCreatedTimestamp,
        accountInfoLastLogin = userLastLogin,
        accountInfoAdmin = userUsername `elem` admins,
        accountInfoCount = c,
        accountInfoStatus = ups
      }
