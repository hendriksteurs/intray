module Intray.Server.Handler.AdminPutAccountSubscriptionSpec
  ( spec,
  )
where

import Intray.API.Gen ()
import Intray.Client
import Intray.Server.TestUtils
import TestImport

spec :: Spec
spec =
  withIntrayServer $
    describe "AdminPutAccountSubscription" $ do
      it "fails without PermitAdminPutAccountSubscription" $ \cenv ->
        forAllValid $ \uuid ->
          forAllValid $ \end ->
            failsWithOutPermission cenv PermitAdminPutAccountSubscription $ \t ->
              clientAdminPutAccountSubscription t uuid end
      it "forbids non-admin users from deleting a user" $ \cenv ->
        forAllValid $ \uuid ->
          forAllValid $ \end ->
            requiresAdmin cenv $ \token ->
              clientAdminPutAccountSubscription token uuid end
      pending "sets the subscription time correctly"
