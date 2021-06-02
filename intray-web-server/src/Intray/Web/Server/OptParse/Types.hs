{-# LANGUAGE OverloadedStrings #-}

module Intray.Web.Server.OptParse.Types where

import Control.Arrow
import Control.Monad.Logger
import Data.Aeson
import Import
import Servant.Client
import YamlParse.Applicative

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
  deriving (Show, Eq)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    objectParser "Configuration" $
      Configuration
        <$> optionalField "port" "The port to serve web requests on"
        <*> optionalFieldWith "api-url" "The url to contact the api server at" (eitherParser (left show . parseBaseUrl) yamlSchema)
        <*> optionalFieldWith "log-level" "The minimal severity of log messages" viaRead
        <*> optionalField "tracking" "The google analytics tracking code"
        <*> optionalField "verification" "The google search console verification code"
        <*> optionalField "login-cache-file" "The file to store the login cache database in"

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
