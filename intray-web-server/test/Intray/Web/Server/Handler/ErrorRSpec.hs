{-# LANGUAGE OverloadedStrings #-}

module Intray.Web.Server.Handler.ErrorRSpec where

import Intray.Data
import Intray.Web.Server.Foundation
import Intray.Web.Server.TestUtils
import Servant.Client
import Test.Syd.Wai
import Test.Syd.Yesod
import TestImport

spec :: Spec
spec = do
  intrayWebServerSpec
    $ ydescribe "ErrorAPIDownR"
    $ yit "gets a 200 for non-logged-in user"
    $ do
      get $ ErrorAPIDownR "example"
      statusIs 200
  managerSpec
    $ aroundWith'
      ( \func man () -> do
          burl <- parseBaseUrl "localhost:8000" -- but this one doesn't exist
          func man (mkClientEnv man burl)
      )
    $ yesodSpecWithSiteSetupFunc' webServerSetupFunc
    $ do
      ydescribe "ErrorAPIDownR"
        $ yit "gets a 200 when the API is down"
        $ do
          get $ ErrorAPIDownR "example"
          statusIs 200
          bodyContains "The Intray API is down."
          bodyContains "example"
      ydescribe "APIDocsR"
        $ yit "redirects to ErrorAPIDownR"
        $ do
          get APIDocsR
          statusIs 303
          loc <- getLocation
          case loc of
            Right (ErrorAPIDownR _) -> do
              void followRedirect
              statusIs 200
            _ -> liftIO $ expectationFailure $ unwords ["Should have redirected:", show loc]
