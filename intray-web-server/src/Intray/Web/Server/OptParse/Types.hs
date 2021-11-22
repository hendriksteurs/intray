{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Web.Server.OptParse.Types where

import Autodocodec
import Control.Arrow
import Control.Monad.Logger
import Data.Aeson (FromJSON, ToJSON)
import Import
import Intray.Client

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype Command
  = CommandServe ServeFlags
  deriving (Show, Eq)

data ServeFlags = ServeFlags
  { serveFlagPort :: !(Maybe Int),
    serveFlagAPIBaseUrl :: !(Maybe BaseUrl),
    serveFlagLogLevel :: !(Maybe LogLevel),
    serveFlagTracking :: !(Maybe Text),
    serveFlagVerification :: !(Maybe Text),
    serveFlagLoginCacheFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq)

data Flags = Flags {flagConfigFile :: !(Maybe FilePath)}
  deriving (Show, Eq)

data Configuration = Configuration
  { confPort :: !(Maybe Int),
    confAPIBaseUrl :: !(Maybe BaseUrl),
    confLogLevel :: !(Maybe LogLevel),
    confTracking :: !(Maybe Text),
    confVerification :: !(Maybe Text),
    confLoginCacheFile :: !(Maybe FilePath)
  }
  deriving stock (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalFieldOrNull "port" "The port to serve web requests on" .= confPort
        <*> optionalFieldOrNullWith "api-url" (bimapCodec (left show . parseBaseUrl) show codec) "The url to contact the api server at" .= confAPIBaseUrl
        <*> optionalFieldOrNull "log-level" "The minimal severity of log messages" .= confLogLevel
        <*> optionalFieldOrNull "tracking" "The google analytics tracking code" .= confTracking
        <*> optionalFieldOrNull "verification" "The google search console verification code" .= confVerification
        <*> optionalFieldOrNull "login-cache-file" "The file to store the login cache database in" .= confLoginCacheFile

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath),
    envPort :: !(Maybe Int),
    envLogLevel :: !(Maybe LogLevel),
    envAPIBaseUrl :: !(Maybe BaseUrl),
    envTracking :: !(Maybe Text),
    envVerification :: !(Maybe Text),
    envLoginCacheFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq)

newtype Dispatch
  = DispatchServe ServeSettings
  deriving (Show)

data ServeSettings = ServeSettings
  { serveSetPort :: !Int,
    serveSetLogLevel :: !LogLevel,
    serveSetAPIBaseUrl :: !BaseUrl,
    serveSetTracking :: !(Maybe Text),
    serveSetVerification :: !(Maybe Text),
    serveSetLoginCacheFile :: !FilePath
  }
  deriving (Show)

data Settings
  = Settings
  deriving (Show, Eq)
