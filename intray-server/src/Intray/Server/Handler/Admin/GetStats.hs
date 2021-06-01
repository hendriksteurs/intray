{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.Admin.GetStats
  ( serveAdminGetStats,
  )
where

import Data.Time
import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.GetAccountInfo
import Intray.Server.Handler.Utils
import Intray.Server.Types

serveAdminGetStats :: AuthCookie -> IntrayHandler AdminStats
serveAdminGetStats AuthCookie {..} = do
  adminStatsNbAccounts <- fmap fromIntegral $ runDb $ count ([] :: [Filter User])
  adminStatsNbItems <- fmap fromIntegral $ runDb $ count ([] :: [Filter IntrayItem])
  now <- liftIO getCurrentTime
  let day :: NominalDiffTime
      day = 86400
  let activeUsers time =
        fmap fromIntegral $ runDb $ count [UserLastLogin >=. Just (addUTCTime (- time) now)]
  activeUsersDaily <- activeUsers day
  activeUsersWeekly <- activeUsers $ 7 * day
  activeUsersMonthly <- activeUsers $ 30 * day
  activeUsersYearly <- activeUsers $ 365 * day
  let adminStatsActiveUsers = ActiveUsers {..}
  adminStatsSubscribedUsers <-
    do
      us <- runDb $ selectList [] []
      fmap (fromIntegral . length . catMaybes) $
        forM us $
          \(Entity _ u) -> getAccountSubscribed (userIdentifier u)
  pure AdminStats {..}
