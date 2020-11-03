{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.Web.Server.Application where

import Intray.Web.Server.Foundation
import Intray.Web.Server.Handler
import Yesod
import Yesod.Auth

mkYesodDispatch "App" resourcesApp
