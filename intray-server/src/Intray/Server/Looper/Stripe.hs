{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.Server.Looper.Stripe
  ( runStripeLooper,
  )
where

import Data.Aeson
import Import
import Intray.Server.Looper.Import
import Intray.Server.Stripe
import Web.Stripe as Stripe

runStripeLooper ::
  FromJSON (StripeReturn a) => StripeRequest a -> Looper (Either StripeError (StripeReturn a))
runStripeLooper request = do
  stripeSets <- asks looperEnvStripeSettings
  liftIO $ runStripeWith stripeSets request
