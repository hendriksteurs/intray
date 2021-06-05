{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Types
  ( ProtectAPI,
    AuthCookie (..),
    Permission (..),
    userPermissions,
    adminPermissions,
    Registration (..),
    LoginForm (..),
    HashedPassword,
    passwordHash,
    validatePassword,
    ItemUUID,
    AccountUUID,
    AccessKeyUUID,
    Username,
    parseUsername,
    parseUsernameWithError,
    usernameText,
    Pricing (..),
    module Data.UUID.Typed,
  )
where

import Data.Aeson as JSON
import Data.Hashable
import Data.Set (Set)
import qualified Data.Text as T
import Data.UUID.Typed
import Import
import Intray.Data
import Servant.Auth
import Servant.Auth.Server
import qualified Web.Stripe.Plan as Stripe

type ProtectAPI = Auth '[JWT] AuthCookie

-- instance OneDoc IntrayAccessKey

data AuthCookie = AuthCookie
  { authCookieUserUUID :: AccountUUID,
    authCookiePermissions :: Set Permission
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AuthCookie

instance ToJSON AuthCookie

instance FromJWT AuthCookie

instance ToJWT AuthCookie

instance FromJWT Permission

instance ToJWT Permission

data Registration = Registration
  { registrationUsername :: Username,
    registrationPassword :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Registration

instance ToJSON Registration where
  toJSON Registration {..} =
    object ["name" .= registrationUsername, "password" .= registrationPassword]

instance FromJSON Registration where
  parseJSON =
    withObject "Registration Text" $ \o -> Registration <$> o .: "name" <*> o .: "password"

data LoginForm = LoginForm
  { loginFormUsername :: Username,
    loginFormPassword :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity LoginForm

instance FromJSON LoginForm where
  parseJSON = withObject "LoginForm" $ \o -> LoginForm <$> o .: "username" <*> o .: "password"

instance ToJSON LoginForm where
  toJSON LoginForm {..} = object ["username" .= loginFormUsername, "password" .= loginFormPassword]

data Pricing = Pricing
  { pricingPlan :: !Stripe.PlanId,
    pricingTrialPeriod :: !(Maybe Int),
    pricingPrice :: !Stripe.Amount,
    pricingCurrency :: !Stripe.Currency,
    pricingStripePublishableKey :: !Text,
    pricingMaxItemsFree :: !Int
  }
  deriving (Show, Eq, Generic)

instance Validity Pricing

instance FromJSON Pricing where
  parseJSON =
    withObject "Pricing" $ \o ->
      Pricing <$> o .: "plan" <*> o .:? "trial-period" <*> o .: "price" <*> o .: "currency"
        <*> o
        .: "publishable-key"
        <*> o
        .: "max-items-free"

instance ToJSON Pricing where
  toJSON Pricing {..} =
    object
      [ "plan" .= pricingPlan,
        "trial-period" .= pricingTrialPeriod,
        "price" .= pricingPrice,
        "currency" .= pricingCurrency,
        "publishable-key" .= pricingStripePublishableKey,
        "max-items-free" .= pricingMaxItemsFree
      ]

instance Validity Stripe.Currency where
  validate = trivialValidation

instance ToJSON Stripe.Currency where
  toJSON c = toJSON $ T.toLower $ T.pack $ show c

deriving instance Validity Stripe.PlanId

deriving instance Hashable Stripe.PlanId

deriving instance ToJSON Stripe.PlanId

deriving instance FromJSON Stripe.PlanId

instance Validity Stripe.Amount where
  validate (Stripe.Amount a) = delve "getAmount" a

instance ToJSON Stripe.Amount where
  toJSON (Stripe.Amount a) = toJSON a

instance FromJSON Stripe.Amount where
  parseJSON v = Stripe.Amount <$> parseJSON v
