{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.GetItems where

import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Utils
import Intray.Server.Item
import Intray.Server.Types

serveGetItems :: AuthCookie -> IntrayHandler [ItemInfo TypedItem]
serveGetItems AuthCookie {..} = do
  itemsEnts <- runDb $ selectList [IntrayItemUserId ==. authCookieUserUUID] [Asc IntrayItemCreated]
  pure $ map (makeItemInfo . entityVal) itemsEnts
