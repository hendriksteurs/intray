{-# LANGUAGE OverloadedStrings #-}

module Intray.Web.Server.Handler.AdminRSpec where

import Intray.Web.Server.Foundation
import Intray.Web.Server.TestUtils
import Test.Syd.Yesod
import TestImport

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec =
  intrayWebServerSpec $
    ydescribe "AdminR" $
      do
        yit "gets a 200 when logged in as admin" $
          withAdminAccount_ $
            do
              get AdminR
              statusIs 200
        yit "gets a 404 when not logged in as admin" $
          withExampleAccount_ $
            do
              get AdminR
              statusIs 404
