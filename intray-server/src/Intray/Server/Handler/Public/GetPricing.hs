{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.Public.GetPricing
  ( serveGetPricing,
  )
where

import Import
import Intray.API
import Intray.Server.OptParse.Types
import Intray.Server.Types

serveGetPricing :: IntrayHandler (Maybe Pricing)
serveGetPricing = do
  mMone <- asks envMonetisation
  forM mMone $ \MonetisationEnv {..} -> do
    let StripeSettings {..} = monetisationEnvStripeSettings
    let pricingPlan = stripeSetPlan
        pricingPrice = monetisationEnvPrice
        pricingStripePublishableKey = stripeSetPublishableKey
        pricingMaxItemsFree = monetisationEnvMaxItemsFree
    pure Pricing {..}
