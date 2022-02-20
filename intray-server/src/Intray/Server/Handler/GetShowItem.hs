{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.GetShowItem where

import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Utils
import Intray.Server.Item
import Intray.Server.Types

serveGetShowItem :: AuthCookie -> IntrayHandler (Maybe (ItemInfo TypedItem))
serveGetShowItem AuthCookie {..} = do
  itemsEnt <- runDB $ selectFirst [IntrayItemUserId ==. authCookieUserUUID] [Asc IntrayItemCreated]
  pure $ makeItemInfo . entityVal <$> itemsEnt
