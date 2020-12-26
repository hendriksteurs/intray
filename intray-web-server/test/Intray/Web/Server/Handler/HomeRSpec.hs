module Intray.Web.Server.Handler.HomeRSpec where

import Intray.Web.Server.Foundation
import Intray.Web.Server.TestUtils
import Test.Syd.Yesod
import TestImport

spec :: Spec
spec =
  intrayWebServerSpec
    $ ydescribe "HomeR"
    $ do
      yit "gets a 200 for non-logged-in user" $ do
        get HomeR
        statusIs 200
      yit "gets a 200 for an example user"
        $ withExampleAccount_
        $ do
          get HomeR
          statusIs 200
