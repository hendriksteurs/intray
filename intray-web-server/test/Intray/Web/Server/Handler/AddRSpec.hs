{-# LANGUAGE OverloadedStrings #-}

module Intray.Web.Server.Handler.AddRSpec where

import Control.Monad.Reader
import Intray.Client
import Intray.Server.TestUtils as TestUtils
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
      let roundtripTest contents =
            yit ("roundtrips " <> show contents)
              $ withExampleAccount
              $ \un pw -> do
                get AddR
                statusIs 200
                request $ do
                  setMethod methodPost
                  setUrl AddR
                  addToken
                  addPostParam "contents" contents
                statusIs 303
                loc <- getLocation
                liftIO $ loc `shouldBe` Right AddR
                man <- asks $ appHttpManager . yesodClientSite
                burl <- asks $ appAPIBaseUrl . yesodClientSite
                let cenv = ClientEnv man burl Nothing
                liftIO $ do
                  token <- TestUtils.login cenv un pw
                  runClientOrError cenv $ do
                    uuids <- clientGetItemUUIDs token
                    case uuids of
                      [uuid] -> do
                        ii <- clientGetItem token uuid
                        liftIO $ itemInfoContents ii `shouldBe` textTypedItem contents
                      _ -> liftIO $ expectationFailure "expected exactly one item"
      roundtripTest "hello"
      roundtripTest "hello\n"
      roundtripTest "hello\nworld"
      roundtripTest "hello\nworld\n"
      roundtripTest "hello\n\nworld"
      roundtripTest "hello\n\nworld\n"
      roundtripTest "hello\r\nworld"
      roundtripTest "hello\r\nworld\n"
