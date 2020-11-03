{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Intray.Server.Handler.DeleteAccount
  ( serveDeleteAccount,
  )
where

import Import
import Intray.API
import Intray.Server.Handler.Utils
import Intray.Server.Types
import Servant

serveDeleteAccount :: AuthCookie -> IntrayHandler NoContent
serveDeleteAccount AuthCookie {..} = do
  deleteAccountFully authCookieUserUUID
  pure NoContent
