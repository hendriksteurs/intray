module Intray.Web.Server.Widget where

import Data.Default
import Import
import Intray.Web.Server.Constants
import Language.Haskell.TH.Syntax (Exp, Q)
import Yesod.Default.Util (WidgetFileSettings, widgetFileNoReload, widgetFileReload)

widgetFile :: String -> Q Exp
widgetFile =
  if development
    then widgetFileReload widgetFileSettings
    else widgetFileNoReload widgetFileSettings

widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
