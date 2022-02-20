{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.AccessKey.GetAccessKey
  ( serveGetAccessKey,
    makeAccessKeyInfo,
  )
where

import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Utils
import Intray.Server.Types
import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth

serveGetAccessKey :: AuthCookie -> AccessKeyUUID -> IntrayHandler AccessKeyInfo
serveGetAccessKey AuthCookie {..} uuid = do
  mac <- runDB $ getBy $ UniqueAccessKeyIdentifier uuid
  case mac of
    Nothing -> throwAll err404 {errBody = "AccessKey not found."}
    Just (Entity _ ak) -> pure $ makeAccessKeyInfo ak

makeAccessKeyInfo :: AccessKey -> AccessKeyInfo
makeAccessKeyInfo AccessKey {..} =
  AccessKeyInfo
    { accessKeyInfoUUID = accessKeyIdentifier,
      accessKeyInfoName = accessKeyName,
      accessKeyInfoCreatedTimestamp = accessKeyCreatedTimestamp,
      accessKeyInfoPermissions = accessKeyPermissions
    }
