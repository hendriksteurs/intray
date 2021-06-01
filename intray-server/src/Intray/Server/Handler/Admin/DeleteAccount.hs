{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.Admin.DeleteAccount
  ( serveAdminDeleteAccount,
  )
where

import Import
import Intray.API
import Intray.Server.Handler.Utils
import Intray.Server.Types
import Servant

serveAdminDeleteAccount :: AuthCookie -> AccountUUID -> IntrayHandler NoContent
serveAdminDeleteAccount AuthCookie {..} uuid = do
  deleteAccountFully uuid
  pure NoContent
