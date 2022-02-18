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

serveAdminPutUserSubscription :: AuthCookie -> Username -> UTCTime -> IntrayHandler NoContent
serveAdminPutUserSubscription AuthCookie {..} username end = do
  _ <-
    runDb $ do
      mUser <- getBy (UniqueUsername username)
      forM_ mUser $ \(Entity _ user) ->
        let uuid = userIdentifier user
         in upsertBy
              (UniqueSubscriptionUser uuid)
              ( Subscription
                  { subscriptionUser = uuid,
                    subscriptionEnd = end
                  }
              )
              [ SubscriptionEnd =. end
              ]
  pure NoContent
