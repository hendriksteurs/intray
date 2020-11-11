{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Intray.Server.Handler.Webhook where

import Control.Monad.Logger
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Import
import Intray.Server.Handler.Utils
import Intray.Server.Types
import Lens.Micro
import Servant
import StripeAPI as Stripe
import System.IO

postStripeWebhookR :: Maybe Text -> Value -> IntrayHandler ()
postStripeWebhookR mHeader payload =
  case mHeader of
    Nothing -> throwError $ err400 {errBody = "No stripe signature."}
    Just _ ->
      -- TODO do verification based on the header.
      case parseMaybe parseJSON payload of
        Nothing -> throwError $ err400 {errBody = "Did not look like a stripe event."}
        Just e ->
          case eventType e of
            "checkout.session.completed" ->
              case parseEither parseJSON (toJSON (notificationEventDataObject (eventData e))) of
                Left err -> throwError err404 {errBody = LB.fromStrict $ TE.encodeUtf8 $ T.pack err}
                Right r -> fullfillOrder r
            t -> do
              logInfoN $ "Not handling event " <> t
              pure ()

fullfillOrder :: Checkout'session -> IntrayHandler ()
fullfillOrder session = do
  let metadata = fromMaybe HM.empty $ checkout'sessionMetadata session
      getMeta k =
        case HM.lookup k metadata >>= parseMaybe parseJSON of
          Nothing -> throwError $ err400 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "Key " <> k <> " not found"}
          Just v -> pure v
  case HM.lookup "product" metadata of
    Just "intray" -> do
      userEmail <- getMeta "user_email" :: IntrayHandler Text
      undefined
    Just (String prod) -> do
      logInfoN $ "Not handling event for different product: " <> prod
      pure () -- Purchase for a different product
    _ -> do
      logInfoN "Not handling event that has no product in its metadata"
      pure ()
