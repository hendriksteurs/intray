{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.Server.Handler.Stripe
  ( PaidStatus (..),
    getUserPaidStatus,
  )
where

import Control.Exception
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Ord
import Data.Time
import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Utils
import Intray.Server.OptParse.Types
import Intray.Server.Types
import Servant
import Servant.Auth.Server

getUserPaidStatus :: AccountUUID -> IntrayHandler PaidStatus
getUserPaidStatus userId = do
  mss <- asks envMonetisation
  case mss of
    Nothing -> pure NoPaymentNecessary
    Just MonetisationEnv {..} -> do
      mu <- runDb $ getBy $ UniqueUserIdentifier userId
      case mu of
        Nothing -> throwAll err404
        Just (Entity _ User {..}) -> do
          isAdmin <- asks ((userUsername `elem`) . envAdmins)
          if isAdmin
            then pure NoPaymentNecessary
            else do
              isFreeloader <- asks ((userUsername `elem`) . envFreeloaders)
              if isFreeloader
                then pure NoPaymentNecessary
                else do
                  mSub <- hasSubscribed monetisationEnvStripeSettings userId
                  case mSub of
                    Just u -> pure $ HasPaid u
                    Nothing -> do
                      c <- runDb $ count [IntrayItemUserId ==. userId]
                      pure $ HasNotPaid (monetisationEnvMaxItemsFree - c)

hasSubscribed :: StripeSettings -> AccountUUID -> IntrayHandler (Maybe UTCTime)
hasSubscribed ss uuid = do
  undefined
--   mc <- runDb $ getBy $ UniqueCustomerUser uuid
--   case mc of
--     Nothing -> pure Nothing -- No such customer on the stripe end, definitely hasn't subscribed then.
--     Just (Entity _ Customer {..}) -> do
--       sl <-
--         runStripeHandlerOrErrorWith ss (Stripe.getSubscriptionsByCustomerId customerStripeCustomer)
--       let relevantSubs =
--             filter
--               ( \s ->
--                   Stripe.planId (Stripe.subscriptionPlan s) == stripeSetPlan ss
--                     && ( Stripe.subscriptionStatus s == Stripe.Active
--                            || Stripe.subscriptionStatus s == Stripe.Trialing
--                        )
--               )
--               $ Stripe.list sl
--       pure $
--         case sortOn Down $ map Stripe.subscriptionCurrentPeriodEnd relevantSubs of
--           [] -> Nothing
--           (end : _) -> Just end
