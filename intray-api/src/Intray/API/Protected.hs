{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Protected
  ( module Intray.API.Protected,
    module Intray.API.Protected.AccessKey,
    module Intray.API.Protected.Account,
    module Intray.API.Protected.Item,
  )
where

import Data.Set (Set)
import Import
import Intray.API.Protected.AccessKey
import Intray.API.Protected.Account
import Intray.API.Protected.Item
import Intray.API.Types
import Servant.API
import Servant.API.Generic

type IntrayProtectedAPI = ToServantApi IntrayProtectedSite

data IntrayProtectedSite route
  = IntrayProtectedSite
      { protectedItemSite :: !(route :- "intray" :> ToServantApi IntrayProtectedItemSite),
        protectedAccountSite :: !(route :- "account" :> ToServantApi IntrayProtectedAccountSite),
        protectedAccessKeySite :: !(route :- "access-key" :> ToServantApi IntrayProtectedAccessKeySite),
        getPermissions :: !(route :- GetPermissions)
      }
  deriving (Generic)

type GetPermissions = ProtectAPI :> "permissions" :> Get '[JSON] (Set Permission)
