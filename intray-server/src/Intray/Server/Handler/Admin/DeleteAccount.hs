{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.Admin.DeleteAccount
  ( serveAdminDeleteAccount,
  )
where

import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Utils
import Intray.Server.Types
import Servant

serveAdminDeleteAccount :: AuthCookie -> Username -> IntrayHandler NoContent
serveAdminDeleteAccount AuthCookie {..} username = do
  mAccount <- runDb $ getBy $ UniqueUsername username
  mapM_ (deleteAccountFully . userIdentifier . entityVal) mAccount
  pure NoContent
