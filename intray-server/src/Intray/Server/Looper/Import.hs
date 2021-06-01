module Intray.Server.Looper.Import
  ( module Intray.Server.Looper.Import,
    module X,
  )
where

import Control.Monad.Logger as X
import Database.Persist.Sqlite
import Import as X
import Intray.Server.OptParse.Types
import Looper as X

type Looper = LoggingT (ReaderT LooperEnv IO)

data LooperEnv = LooperEnv
  { looperEnvStripeSettings :: !StripeSettings,
    looperEnvConnectionPool :: !ConnectionPool
  }
  deriving (Show)
