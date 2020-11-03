{-# LANGUAGE TemplateHaskell #-}

module Intray.Web.Server.Handler.Error
  ( getErrorAPIDownR,
  )
where

import qualified Data.Text as T
import Import
import Intray.Web.Server.Foundation
import Text.Read (readMaybe)
import Yesod

getErrorAPIDownR :: Text -> Handler Html
getErrorAPIDownR e = do
  let ms = readMaybe (T.unpack e) :: Maybe String
  withNavBar $(widgetFile "api-down")
