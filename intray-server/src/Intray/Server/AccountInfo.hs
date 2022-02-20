{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.AccountInfo
  ( getAccountInfoForUser,
  )
where

import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Stripe
import Intray.Server.Handler.Utils
import Intray.Server.Types

getAccountInfoForUser :: User -> IntrayHandler AccountInfo
getAccountInfoForUser User {..} = do
  admins <- asks envAdmins
  c <- runDB $ count ([IntrayItemUserId ==. userIdentifier] :: [Filter IntrayItem])
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
