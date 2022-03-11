module Intray.Server.Types
  ( module Intray.Server.Types,
    MonetisationSettings (..),
  )
where

import Control.Monad.Logger
import Database.Persist.Sqlite
import Import
import Intray.API
import Intray.Server.OptParse.Types
import Servant
import Servant.Auth.Server

type LF = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

data IntrayServerEnv = IntrayServerEnv
  { envHost :: !Text,
    envLogFunc :: !LF,
    envConnectionPool :: !ConnectionPool,
    envCookieSettings :: !CookieSettings,
    envJWTSettings :: !JWTSettings,
    envAdmins :: ![Username],
    envFreeloaders :: ![Username],
    envMonetisation :: !(Maybe MonetisationEnv)
  }

data MonetisationEnv = MonetisationEnv
  { monetisationEnvStripeSettings :: StripeSettings,
    monetisationEnvMaxItemsFree :: !Int,
    monetisationEnvPrice :: !Text
  }

type IntrayHandler = ReaderT IntrayServerEnv (LoggingT Handler)
