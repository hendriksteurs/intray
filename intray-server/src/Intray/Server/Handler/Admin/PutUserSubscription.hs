{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.Admin.PutUserSubscription
  ( serveAdminPutUserSubscription,
  )
where

import Data.Time
import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Utils
import Intray.Server.Types
import Servant

serveAdminPutUserSubscription :: AuthCookie -> AccountUUID -> UTCTime -> IntrayHandler NoContent
serveAdminPutUserSubscription AuthCookie {..} uuid end = do
  _ <-
    runDb $
      upsertBy
        (UniqueSubscriptionUser uuid)
        ( Subscription
            { subscriptionUser = uuid,
              subscriptionEnd = end
            }
        )
        [ SubscriptionEnd =. end
        ]
  pure NoContent
