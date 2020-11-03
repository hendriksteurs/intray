{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.Data.Stripe where

import Database.Persist
import Database.Persist.Sql
import Web.Stripe.Types as Stripe

deriving instance PersistField CustomerId

deriving instance PersistFieldSql CustomerId

deriving instance PersistField EventId

deriving instance PersistFieldSql EventId
