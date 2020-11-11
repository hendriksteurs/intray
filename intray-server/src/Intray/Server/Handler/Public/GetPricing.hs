{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.Public.GetPricing
  ( serveGetPricing,
  )
where

import Data.Cache as Cache
import Import
import Intray.API
import Intray.Server.OptParse.Types
import Intray.Server.Types
import StripeAPI as Stripe

serveGetPricing :: IntrayHandler (Maybe Pricing)
serveGetPricing = do
  mMone <- asks envMonetisation
  forM mMone $ \MonetisationEnv {..} -> do
    let StripeSettings {..} = monetisationEnvStripeSettings
    mPlan <- liftIO $ Cache.lookup monetisationEnvPlanCache stripeSetPlan
    Stripe.Plan {..} <-
      case mPlan of
        Nothing -> do
          plan <- undefined -- runStripeHandlerOrErrorWith monetisationEnvStripeSettings $ getPlan stripeSetPlan
          liftIO $ Cache.insert monetisationEnvPlanCache stripeSetPlan plan
          pure plan
        Just plan -> pure plan
    let pricingPlan = stripeSetPlan
        pricingTrialPeriod = planTrialPeriodDays
        pricingPrice = undefined -- Stripe.Amount planAmount
        pricingCurrency = planCurrency
        pricingStripePublishableKey = stripeSetPublishableKey
        pricingMaxItemsFree = monetisationEnvMaxItemsFree
    pure Pricing {..}
