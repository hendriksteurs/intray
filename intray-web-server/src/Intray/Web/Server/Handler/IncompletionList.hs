{-# LANGUAGE TemplateHaskell #-}

module Intray.Web.Server.Handler.IncompletionList
  ( getIncompletionListR
  ) where

import Intray.Web.Server.Foundation
import Yesod

getIncompletionListR :: Handler Html
getIncompletionListR = do
  neverExpires
  withNavBar $(widgetFile "incompletion-list")
