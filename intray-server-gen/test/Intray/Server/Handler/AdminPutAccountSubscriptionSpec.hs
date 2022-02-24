module Intray.Server.Handler.AdminPutAccountSubscriptionSpec
  ( spec,
  )
where

import Intray.API.Gen ()
import Intray.Client
import Intray.Server.TestUtils
import TestImport

spec :: Spec
spec = do
  withIntrayServer $
    describe "AdminPutAccountSubscription" $ do
      it "fails without PermitAdminPutAccountSubscription" $ \cenv ->
        forAllValid $ \username ->
          forAllValid $ \end ->
            failsWithOutPermission cenv PermitAdminPutAccountSubscription $ \t ->
              clientAdminPutAccountSubscription t username end
      it "forbids non-admin users from deleting a user" $ \cenv ->
        forAllValid $ \username ->
          forAllValid $ \end ->
            requiresAdmin cenv $ \token ->
              clientAdminPutAccountSubscription token username end
  withFreeIntrayServer $
    it "sets the subscription time correctly" $ \cenv ->
      forAllValid $ \end ->
        withAdmin cenv $ \adminToken ->
          withValidNewUserAndData cenv $ \username _ userToken ->
            runClientOrError cenv $ do
              NoContent <- clientAdminPutAccountSubscription adminToken username end
              accountInfo <- clientGetAccountInfo userToken
              liftIO $ accountInfoStatus accountInfo `shouldBe` NoPaymentNecessary
  withPaidIntrayServer 5 $
    it "sets the subscription time correctly" $ \cenv ->
      forAllValid $ \end ->
        withAdmin cenv $ \adminToken ->
          withValidNewUserAndData cenv $ \username _ userToken ->
            runClientOrError cenv $ do
              NoContent <- clientAdminPutAccountSubscription adminToken username end
              accountInfo <- clientGetAccountInfo userToken
              liftIO $ accountInfoStatus accountInfo `shouldBe` HasPaid end
