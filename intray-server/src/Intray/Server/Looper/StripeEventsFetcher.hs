{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Looper.StripeEventsFetcher where

import Conduit
import Data.Aeson
import qualified Data.Text as T
import Database.Persist
import Intray.Data
import Intray.Server.Looper.DB
import Intray.Server.Looper.Import
import Intray.Server.Looper.Stripe
import Intray.Server.OptParse.Types
import Web.Stripe as Stripe ((-&-))
import qualified Web.Stripe as Stripe
import Web.Stripe.Conduit
import qualified Web.Stripe.Customer as Stripe
import qualified Web.Stripe.Event as Stripe
import qualified Web.Stripe.Session as Stripe

stripeEventsFetcherLooper :: Looper ()
stripeEventsFetcherLooper = do
  stripeConfig <- asks (stripeSetStripeConfig . looperEnvStripeSettings)
  let fetchConduit =
        stripeConduit
          stripeConfig
          (Stripe.getEvents -&- Stripe.CheckoutSessionCompletedEvent)
          Stripe.eventId
  runConduit $ fetchConduit .| dealWithEventC

dealWithEventC :: ConduitT Stripe.Event Void Looper ()
dealWithEventC = do
  me <- await
  case me of
    Nothing -> pure ()
    Just e -> do
      lift $ dealWithEvent e
      dealWithEventC

dealWithEvent :: Stripe.Event -> Looper ()
dealWithEvent e = do
  mse <- looperDB $ getBy $ UniqueStripeEvent $ Stripe.eventId e
  case mse of
    Just _ -> pure () -- No need to re-do this
    Nothing -> do
      se <- handleEvent e
      looperDB $ insert_ se

handleEvent :: Stripe.Event -> Looper StripeEvent
handleEvent Stripe.Event {..} =
  let err t = do
        logErr t
        pure $ StripeEvent {stripeEventEvent = eventId, stripeEventError = Just t}
   in case eventType of
        Stripe.CheckoutSessionCompletedEvent ->
          case eventData of
            Stripe.CheckoutEvent Stripe.Session {..} ->
              case sessionData of
                Stripe.SessionSubscription eCus _ ->
                  case sessionClientReferenceId of
                    Just crid ->
                      case parseUUIDText (Stripe.getClientReferenceId crid) of
                        Just au ->
                          completePayment eventId au $
                            case eCus of
                              Stripe.Id cid -> cid
                              Stripe.Expanded c -> Stripe.customerId c
                        Nothing -> err "Client reference id didn't look like an AccountUUID"
                    Nothing -> err "No client reference id"
                _ -> err "Unknown session mode"
            _ -> err "Unknown event data"
        _ -> err "Unknown event"

completePayment :: Stripe.EventId -> AccountUUID -> Stripe.CustomerId -> Looper StripeEvent
completePayment eventId account cid = do
  void $
    looperDB $
      upsertBy
        (UniqueStripeCustomer account cid)
        (StripeCustomer {stripeCustomerUser = account, stripeCustomerCustomer = cid})
        [StripeCustomerCustomer =. cid]
  pure StripeEvent {stripeEventEvent = eventId, stripeEventError = Nothing}

looperStripeOrErr ::
  FromJSON (Stripe.StripeReturn a) =>
  Stripe.StripeRequest a ->
  (Stripe.StripeReturn a -> Looper ()) ->
  Looper ()
looperStripeOrErr r func = do
  errOrRes <- runStripeLooper r
  case errOrRes of
    Left err -> logErr $ T.pack $ unlines ["Stripe responded with an error: ", ppShow err]
    Right res -> func res

logErr :: Text -> Looper ()
logErr = logErrorNS "stripe-events-fetcher"
