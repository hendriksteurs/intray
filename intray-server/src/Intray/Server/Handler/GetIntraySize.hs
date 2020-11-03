{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Intray.Server.Handler.GetIntraySize
  ( serveGetIntraySize,
  )
where

import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Utils
import Intray.Server.Types

serveGetIntraySize :: AuthCookie -> IntrayHandler Int
serveGetIntraySize AuthCookie {..} = runDb $ count [IntrayItemUserId ==. authCookieUserUUID]
