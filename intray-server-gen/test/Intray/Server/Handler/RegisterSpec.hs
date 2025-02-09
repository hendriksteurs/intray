{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Server.Handler.RegisterSpec
  ( spec,
  )
where

import Intray.API
import Intray.API.Gen ()
import Intray.Client
import Intray.Server.TestUtils
import Network.HTTP.Types as HTTP
import TestImport

spec :: Spec
spec =
  withIntrayServer
    $ describe "Register"
    $ do
      describe "make user"
        $ it "does not crash"
        $ \cenv ->
          forAllValid $ \registration -> do
            nameOrError <- runClient cenv $ clientPostRegister registration
            case nameOrError of
              Right NoContent -> pure ()
              Left err ->
                let snf = expectationFailure $ "Should not fail with error: " <> show err
                 in case err of
                      FailureResponse _ resp ->
                        if HTTP.statusCode (responseStatusCode resp) == 409
                          then pure ()
                          else snf
                      _ -> snf
      describe "duplicated users"
        $ it "returns err409 when the username already exists"
        $ \cenv ->
          forAllValid $ \(password, registration) -> do
            void $ runClient cenv $ clientPostRegister registration
            nameOrError <-
              runClient cenv . clientPostRegister $
                Registration (registrationUsername registration) password
            case nameOrError of
              Left err ->
                let snf = expectationFailure $ "Should not fail with error: " <> show err
                 in case err of
                      FailureResponse _ resp ->
                        if HTTP.statusCode (responseStatusCode resp) == 409
                          then pure ()
                          else snf
                      _ -> snf
              Right _ -> expectationFailure "Should not have succeeded."
