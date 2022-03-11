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
serveAdminDeleteAccount _ username = do
  mAccount <- runDB $ getBy $ UniqueUsername username
  mapM_ (deleteAccountFully . userIdentifier . entityVal) mAccount
  pure NoContent
