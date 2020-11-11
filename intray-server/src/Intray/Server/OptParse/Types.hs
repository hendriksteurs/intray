{-# LANGUAGE OverloadedStrings #-}

module Intray.Server.OptParse.Types where

import Control.Monad.Logger
import Data.Aeson hiding (object)
import Database.Persist.Sqlite
import Import
import Intray.API
import YamlParse.Applicative

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype Command
  = CommandServe ServeFlags
  deriving (Show, Eq)

data ServeFlags
  = ServeFlags
      { serveFlagHost :: !(Maybe String),
        serveFlagPort :: !(Maybe Int),
        serveFlagDb :: !(Maybe Text),
        serveFlagAdmins :: ![String],
        serveFlagFreeloaders :: ![String],
        serveFlagLogLevel :: Maybe LogLevel,
        serveFlagSigningKeyFile :: !(Maybe FilePath),
        serveFlagStripePlan :: !(Maybe String),
        serveFlagStripeSecretKey :: !(Maybe String),
        serveFlagStripePublishableKey :: !(Maybe String),
        serveFlagMaxItemsFree :: !(Maybe Int)
      }
  deriving (Show, Eq)

data Flags
  = Flags
      { flagConfigFile :: !(Maybe FilePath)
      }
  deriving (Show, Eq)

data Configuration
  = Configuration
      { confHost :: !(Maybe String),
        confPort :: !(Maybe Int),
        confDb :: !(Maybe Text),
        confAdmins :: !(Maybe [String]),
        confFreeloaders :: !(Maybe [String]),
        confLogLevel :: !(Maybe LogLevel),
        confSigningKeyFile :: !(Maybe FilePath),
        confMonetisationConfig :: !(Maybe MonetisationConfiguration)
      }
  deriving (Show, Eq)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    objectParser "Configuration" $
      Configuration <$> optionalField "api-host" "The host to serve the api-server on"
        <*> optionalField "api-port" "The port to serve the api-server on"
        <*> optionalField "database" "The database file"
        <*> optionalField "admins" "The list of usernames that will be considered administrators"
        <*> optionalField "freeloaders" "The list of usernames that won't have to pay"
        <*> optionalFieldWith "log-level" "The minimal log level for log messages" viaRead
        <*> optionalField "log-level" "The file to store the JWT signing key in"
        <*> optionalField
          "monetisation"
          "Monetisation configuration. If this is not configured then the server is run for free."

data MonetisationConfiguration
  = MonetisationConfiguration
      { monetisationConfStripePlan :: !(Maybe String),
        monetisationConfStripeSecretKey :: !(Maybe String),
        monetisationConfStripePublishableKey :: !(Maybe String),
        monetisationConfMaxItemsFree :: !(Maybe Int)
      }
  deriving (Show, Eq)

instance FromJSON MonetisationConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema MonetisationConfiguration where
  yamlSchema =
    objectParser "MonetisationConfiguration" $
      MonetisationConfiguration
        <$> optionalField
          "stripe-plan"
          "The stripe identifier of the stripe plan used to checkout a subscription"
        <*> optionalField "stripe-secret-key" "The secret key for calling the stripe api"
        <*> optionalField "stripe-publishable-key" "The publishable key for calling the stripe api"
        <*> optionalField "max-items-free" "The number of items a free user can have on the server"

data Environment
  = Environment
      { envConfigFile :: !(Maybe FilePath),
        envHost :: !(Maybe String),
        envPort :: !(Maybe Int),
        envDb :: !(Maybe Text),
        envLogLevel :: !(Maybe LogLevel),
        envSigningKeyFile :: !(Maybe FilePath),
        envStripePlan :: !(Maybe String),
        envStripeSecretKey :: !(Maybe String),
        envStripePublishableKey :: !(Maybe String),
        envMaxItemsFree :: !(Maybe Int)
      }
  deriving (Show, Eq)

newtype Dispatch
  = DispatchServe ServeSettings
  deriving (Show)

data Settings
  = Settings
  deriving (Show, Eq)

data ServeSettings
  = ServeSettings
      { serveSetHost :: !Text,
        serveSetPort :: !Int,
        serveSetLogLevel :: !LogLevel,
        serveSetSigningKeyFile :: !(Path Abs File),
        serveSetConnectionInfo :: !SqliteConnectionInfo,
        serveSetAdmins :: ![Username],
        serveSetFreeloaders :: ![Username],
        serveSetMonetisationSettings :: !(Maybe MonetisationSettings)
      }
  deriving (Show)

data MonetisationSettings
  = MonetisationSettings
      { monetisationSetStripeSettings :: !StripeSettings,
        monetisationSetMaxItemsFree :: !Int
      }
  deriving (Show)

data StripeSettings
  = StripeSettings
      { stripeSetPlan :: !Text,
        stripeSetPublishableKey :: !Text,
        stripeSetSecretKey :: !Text
      }
  deriving (Show)
