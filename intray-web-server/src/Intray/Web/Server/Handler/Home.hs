{-# LANGUAGE TemplateHaskell #-}

module Intray.Web.Server.Handler.Home
  ( getHomeR
  ) where

import Import
import Intray.Client
import Intray.Web.Server.Foundation
import Intray.Web.Server.Handler.Pricing
import Yesod

getHomeR :: Handler Html
getHomeR = do
  mPricing <- runClientOrErr clientGetPricing
  withNavBar $(widgetFile "home")
