{-# LANGUAGE TemplateHaskell #-}

module Intray.Web.Server.Handler.Checkout
  ( getCheckoutR,
    getCheckoutSuccessR,
    getCheckoutCanceledR,
  )
where

import Intray.Client
import Intray.Web.Server.Foundation
import Yesod

getCheckoutR :: Handler Html
getCheckoutR = do
  withLogin $ \t -> do
    mMonetisation <- runClientOrErr clientGetMonetisation
    status <- runClientOrErr $ clientGetUserSubscription t
    renderUrl <- getUrlRender
    InitiatedCheckoutSession {..} <-
      runClientOrErr $
        clientPostInitiateStripeCheckoutSession
          t
          InitiateStripeCheckoutSession
            { initiateStripeCheckoutSessionSuccessUrl = renderUrl CheckoutSuccessR,
              initiateStripeCheckoutSessionCanceledUrl = renderUrl CheckoutCanceledR
            }
    let stripeForm Monetisation {..} = $(widgetFile "stripe-form")
    now <- liftIO getCurrentTime
    withNavBar $(widgetFile "checkout")

getCheckoutSuccessR :: Handler Html
getCheckoutSuccessR = redirect AccountR

getCheckoutCanceledR :: Handler Html
getCheckoutCanceledR = redirect AccountR
