{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Intray.Server.Handler.AccessKey.GetAccessKeys
  ( serveGetAccessKeys,
  )
where

import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.AccessKey.GetAccessKey (makeAccessKeyInfo)
import Intray.Server.Handler.Utils
import Intray.Server.Types

serveGetAccessKeys :: AuthCookie -> IntrayHandler [AccessKeyInfo]
serveGetAccessKeys AuthCookie {..} = do
  aks <- runDb $ selectList [AccessKeyUser ==. authCookieUserUUID] []
  pure $ map (makeAccessKeyInfo . entityVal) aks
