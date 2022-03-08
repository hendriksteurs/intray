{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.Server.Handler.Stripe
  ( runStripeHandler,
    runStripeHandlerOrError,
    runStripeHandlerOrErrorWith,
    PaidStatus (..),
    getUserPaidStatus,
  )
where

import Control.Exception
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Time
import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Utils
import Intray.Server.OptParse.Types
import Intray.Server.Stripe
import Intray.Server.Types
import Servant
import Servant.Auth.Server
import Web.Stripe as Stripe (StripeError, StripeRequest, StripeReturn)

runStripeHandler ::
  FromJSON (StripeReturn a) =>
  StripeRequest a ->
  IntrayHandler (Maybe (Either StripeError (StripeReturn a)))
runStripeHandler request = do
  mStripeSets <- asks (fmap monetisationEnvStripeSettings . envMonetisation)
  forM mStripeSets $ \ms -> liftIO $ runStripeWith ms request

runStripeHandlerOrError ::
  FromJSON (StripeReturn a) => StripeRequest a -> IntrayHandler (Maybe (StripeReturn a))
runStripeHandlerOrError request = do
  mStripeSets <- asks (fmap monetisationEnvStripeSettings . envMonetisation)
  forM mStripeSets $ \ms -> runStripeHandlerOrErrorWith ms request

runStripeHandlerOrErrorWith ::
  FromJSON (StripeReturn a) =>
  StripeSettings ->
  StripeRequest a ->
  IntrayHandler (StripeReturn a)
runStripeHandlerOrErrorWith ms request = do
  errOrRes <- liftIO $ runStripeWith ms request
  case errOrRes of
    Left err -> throwError (err503 {errBody = LB8.pack $ displayException err})
    Right res -> pure res

getUserPaidStatus :: AccountUUID -> IntrayHandler PaidStatus
getUserPaidStatus userId = do
  mss <- asks envMonetisation
  case mss of
    Nothing -> pure NoPaymentNecessary
    Just MonetisationEnv {..} -> do
      mu <- runDB $ getBy $ UniqueUserIdentifier userId
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
                  mSub <- hasSubscribed userId
                  case mSub of
                    Just u -> pure $ HasPaid u
                    Nothing -> do
                      c <- runDB $ count [IntrayItemUserId ==. userId]
                      pure $ HasNotPaid (monetisationEnvMaxItemsFree - c)

hasSubscribed :: AccountUUID -> IntrayHandler (Maybe UTCTime)
hasSubscribed uuid = runDB $ fmap (fmap (subscriptionEnd . entityVal)) $ getBy $ UniqueSubscriptionUser uuid
