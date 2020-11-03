{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Server.Handler.AdminGetStatsSpec
  ( spec,
  )
where

import Intray.Client
import Intray.Data.Gen ()
import Intray.Server.TestUtils
import TestImport

spec :: Spec
spec =
  withIntrayServer
    $ describe "AdminGetStats"
    $ do
      it "fails without PermitAdminGetStats" $ \cenv ->
        failsWithOutPermission cenv PermitAdminGetStats clientAdminGetStats
      it "forbids non-admin users from fetching admin stats" $ \cenv ->
        requiresAdmin cenv clientAdminGetStats
      it "returns valid admin stats" $ \cenv ->
        withAdmin cenv $ \token -> do
          adminStats <- runClientOrError cenv $ clientAdminGetStats token
          shouldBeValid adminStats
