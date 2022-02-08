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
      forAllValid $ \accountUuid ->
        failsWithOutPermission cenv PermitAdminGetAccount $ \token ->
          clientAdminGetAccount token accountUuid
    it "forbids non-admin users from getting account info" $ \cenv ->
      forAllValid $ \accountUuid ->
        requiresAdmin cenv $ \token ->
          clientAdminGetAccount token accountUuid
    it "returns the same account info as when a user logs in" $ \cenv ->
      withValidNewUserAndData cenv $ \_ _ userToken ->
        withAdmin cenv $ \adminToken ->
          runClientOrError cenv $ do
            accountInfoViaUser <- clientGetAccountInfo userToken
            accountInfoViaAdmin <- clientAdminGetAccount adminToken (accountInfoUUID accountInfoViaUser)
            liftIO $ accountInfoViaAdmin `shouldBe` accountInfoViaUser
