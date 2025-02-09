{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

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
  runDb $ deleteWhere [AccessKeyIdentifier ==. uuid]
  pure NoContent
