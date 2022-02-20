{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.AccessKey.DeleteAccessKey
  ( serveDeleteAccessKey,
  )
where

import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Utils
import Intray.Server.Types
import Servant

serveDeleteAccessKey :: AuthCookie -> AccessKeyUUID -> IntrayHandler NoContent
serveDeleteAccessKey AuthCookie {..} uuid = do
  runDB $ deleteWhere [AccessKeyIdentifier ==. uuid]
  pure NoContent
