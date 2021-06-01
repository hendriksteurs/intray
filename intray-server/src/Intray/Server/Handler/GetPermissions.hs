{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.GetPermissions where

import Data.Set (Set)
import Import
import Intray.API
import Intray.Server.Types

serveGetPermissions :: AuthCookie -> IntrayHandler (Set Permission)
serveGetPermissions AuthCookie {..} = pure authCookiePermissions
