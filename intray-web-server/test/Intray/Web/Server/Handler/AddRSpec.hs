{-# LANGUAGE OverloadedStrings #-}

module Intray.Web.Server.Handler.AddRSpec where

import Intray.Web.Server.Foundation
import Intray.Web.Server.TestUtils
import Network.HTTP.Types
import Test.Syd.Yesod
import TestImport

spec :: Spec
spec =
  intrayWebServerSpec
    $ ydescribe "AddR"
    $ do
      yit "gets a 200 for an example user"
        $ withExampleAccount_
        $ do
          get AddR
          statusIs 200
      yit "manages to add an item with a newline"
        $ withExampleAccount_
        $ do
          get AddR
          statusIs 200
          request $ do
            setMethod methodPost
            setUrl AddR
            addToken
            addPostParam "contents" "hello\nworld"
          statusIs 303
          loc <- getLocation
          liftIO $ loc `shouldBe` Right AddR
