{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Types
    ( ProtectAPI
    , AuthCookie(..)
    , Permission(..)
    , userPermissions
    , adminPermissions
    , Registration(..)
    , LoginForm(..)
    , GetDocsResponse(..)
    , HashedPassword
    , passwordHash
    , validatePassword
    , ItemUUID
    , AccountUUID
    , AccessKeyUUID
    , Username
    , parseUsername
    , parseUsernameWithError
    , usernameText
    , module Data.UUID.Typed
    ) where

import Import

import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text.Encoding as TE
import Data.Time
import qualified Data.UUID as UUID
import Data.UUID.Typed
import System.IO.Unsafe

import Text.Blaze as HTML
import Text.Blaze.Html as HTML

import Web.Cookie

import Servant.API
import Servant.Auth
import Servant.Auth.Docs ()
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Docs
import Servant.HTML.Blaze

import Intray.Data

type ProtectAPI = Auth '[ JWT] AuthCookie

data AuthCookie = AuthCookie
    { authCookieUserUUID :: AccountUUID
    , authCookiePermissions :: Set Permission
    } deriving (Show, Eq, Ord, Generic)

instance FromJSON AuthCookie

instance ToJSON AuthCookie

instance FromJWT AuthCookie

instance ToJWT AuthCookie

instance FromJWT Permission

instance ToJWT Permission

instance ToCapture (Capture "uuid" ItemUUID) where
    toCapture _ = DocCapture "uuid" "The UUID of the item"

instance ToCapture (Capture "uuid" AccountUUID) where
    toCapture _ = DocCapture "uuid" "The UUID of the account"

instance ToCapture (Capture "uuid" AccessKeyUUID) where
    toCapture _ = DocCapture "uuid" "The UUID of the access key"

instance ToSample UTCTime where
    toSamples Proxy = singleSample $ UTCTime (fromGregorian 2018 2 10) 42

instance ToSample Text where
    toSamples Proxy = singleSample "Example Text"

instance ToSample (UUID a) where
    toSamples Proxy = singleSample (UUID $ UUID.fromWords 0 0 0 0)

instance ToSample Int where
    toSamples Proxy = singleSample 42

data Registration = Registration
    { registrationUsername :: Username
    , registrationPassword :: Text
    } deriving (Show, Eq, Ord, Generic)

instance Validity Registration

instance ToJSON Registration where
    toJSON Registration {..} =
        object
            ["name" .= registrationUsername, "password" .= registrationPassword]

instance FromJSON Registration where
    parseJSON =
        withObject "Registration Text" $ \o ->
            Registration <$> o .: "name" <*> o .: "password"

instance ToSample Registration

data LoginForm = LoginForm
    { loginFormUsername :: Username
    , loginFormPassword :: Text
    } deriving (Show, Eq, Ord, Generic)

instance Validity LoginForm

instance FromJSON LoginForm where
    parseJSON =
        withObject "LoginForm" $ \o ->
            LoginForm <$> o .: "username" <*> o .: "password"

instance ToJSON LoginForm where
    toJSON LoginForm {..} =
        object
            ["username" .= loginFormUsername, "password" .= loginFormPassword]

instance ToSample LoginForm

instance ToSample Username

instance ToSample SetCookie where
    toSamples Proxy = singleSample def

newtype GetDocsResponse = GetDocsResponse
    { unGetDocsResponse :: HTML.Html
    } deriving (Generic)

instance MimeUnrender HTML GetDocsResponse where
    mimeUnrender Proxy bs =
        Right $ GetDocsResponse $ HTML.unsafeLazyByteString bs

instance ToSample GetDocsResponse where
    toSamples Proxy = singleSample $ GetDocsResponse "Documentation (In HTML)."

instance ToMarkup GetDocsResponse where
    toMarkup (GetDocsResponse html) = toMarkup html

instance ToSample Permission

instance (Ord a, ToSample a) => ToSample (Set a) where
    toSamples Proxy = second S.fromList <$> toSamples Proxy

instance ToSample AccessKeySecret where
    toSamples Proxy =
        singleSample $ unsafePerformIO generateRandomAccessKeySecret
