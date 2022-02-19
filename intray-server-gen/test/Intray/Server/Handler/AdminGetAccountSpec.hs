{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Server.Handler.AdminGetAccountSpec
  ( spec,
  )
where

import Intray.Client
import Intray.Data.Gen ()
import Intray.Server.TestUtils
import TestImport

spec :: Spec
spec = withIntrayServer $
  describe "AdminGetAccount" $ do
    it "fails without PermitAdminGetAccount" $ \cenv ->
      forAllValid $ \username ->
        failsWithOutPermission cenv PermitAdminGetAccount $ \token ->
          clientAdminGetAccount token username
    it "forbids non-admin users from getting account info" $ \cenv ->
      forAllValid $ \username ->
        requiresAdmin cenv $ \token ->
          clientAdminGetAccount token username
    it "returns the same account info as when a user logs in" $ \cenv ->
      withValidNewUserAndData cenv $ \_ _ userToken ->
        withAdmin cenv $ \adminToken ->
          runClientOrError cenv $ do
            accountInfoViaUser <- clientGetAccountInfo userToken
            accountInfoViaAdmin <- clientAdminGetAccount adminToken (accountInfoUsername accountInfoViaUser)
            liftIO $ accountInfoViaAdmin `shouldBe` accountInfoViaUser
