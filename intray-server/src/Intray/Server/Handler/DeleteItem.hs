{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Intray.Server.Handler.DeleteItem
  ( serveDeleteItem,
  )
where

import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Utils
import Intray.Server.Types
import Servant

serveDeleteItem :: AuthCookie -> ItemUUID -> IntrayHandler NoContent
serveDeleteItem AuthCookie {..} id_ = do
  runDb . deleteBy $ UniqueItemIdentifier id_
  pure NoContent
