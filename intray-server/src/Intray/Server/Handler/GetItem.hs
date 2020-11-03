{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Intray.Server.Handler.GetItem
  ( serveGetItem,
  )
where

import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Utils
import Intray.Server.Item
import Intray.Server.Types
import Servant

serveGetItem :: AuthCookie -> ItemUUID -> IntrayHandler (ItemInfo TypedItem)
serveGetItem AuthCookie {..} id_ = do
  mitem <- runDb $ getBy $ UniqueItemIdentifier id_
  case mitem of
    Nothing -> throwError err404 {errBody = "Item not found."}
    Just item -> pure $ makeItemInfo $ entityVal item
