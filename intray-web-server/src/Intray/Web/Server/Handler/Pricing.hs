{-# LANGUAGE RecordWildCards #-}

module Intray.Web.Server.Handler.Pricing
  ( pricingShowAmountPerYear,
    showAmountForPricing,
  )
where

import Import
import Intray.Client
import Web.Stripe as Stripe
import Web.Stripe.Types as Stripe

pricingShowAmountPerYear :: Pricing -> String
pricingShowAmountPerYear Pricing {..} = showAmountForPricing pricingCurrency pricingPrice

showAmountForPricing :: Currency -> Amount -> String
showAmountForPricing cur (Amount i) =
  let (q, r) = quotRem i 100
      hundred =
        if r == 0
          then unwords [show q, show cur]
          else showAmount cur i
   in case cur of
        EUR -> hundred
        USD -> hundred
        CHF -> hundred
        _ -> showAmount cur i

quotPrice :: Amount -> Int -> Amount
quotPrice (Amount i) d = Amount $ i `quot` d
