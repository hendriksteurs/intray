module Intray.Server.Handler.AdminPutUserSubscriptionSpec
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
    describe "AdminPutUserSubscription" $ do
      it "fails without PermitAdminPutUserSubscription" $ \cenv ->
        forAllValid $ \uuid ->
          forAllValid $ \end ->
            failsWithOutPermission cenv PermitAdminPutUserSubscription $ \t ->
              clientAdminPutUserSubscription t uuid end
      it "forbids non-admin users from deleting a user" $ \cenv ->
        forAllValid $ \uuid ->
          forAllValid $ \end ->
            requiresAdmin cenv $ \token ->
              clientAdminPutUserSubscription token uuid end
      pending "sets the subscription time correctly"
