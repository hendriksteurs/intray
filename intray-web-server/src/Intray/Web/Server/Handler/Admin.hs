{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Intray.Web.Server.Handler.Admin
  ( getAdminR,
    getAdminAccountR,
    postAdminAccountDeleteR,
  )
where

import Data.Time
import Import
import Intray.Client
import Intray.Web.Server.Foundation
import Intray.Web.Server.Time
import Text.Time.Pretty
import Yesod

getAdminR :: Handler Html
getAdminR =
  withAdminCreds $ \t -> do
    AdminStats {..} <- runClientOrErr $ clientAdminGetStats t
    mPricing <- runClientOrErr clientGetPricing
    let ActiveUsers {..} = adminStatsActiveUsers
    users <- runClientOrErr $ clientAdminGetAccounts t
    now <- liftIO getCurrentTime
    token <- genToken
    withNavBar $(widgetFile "admin")

getAdminAccountR :: Username -> Handler Html
getAdminAccountR uuid = withAdminCreds $ \t -> do
  AccountInfo {..} <- runClientOrErr $ clientAdminGetAccount t uuid
  now <- liftIO getCurrentTime
  withNavBar $(widgetFile "admin/account")

postAdminAccountDeleteR :: Username -> Handler Html
postAdminAccountDeleteR uuid =
  withAdminCreds $ \t -> do
    NoContent <- runClientOrErr $ clientAdminDeleteAccount t uuid
    redirect AdminR

withAdminCreds :: (Token -> Handler Html) -> Handler Html
withAdminCreds func =
  withLogin $ \t -> do
    adminInfo <- runClientOrErr $ clientGetAccountInfo t
    if accountInfoAdmin adminInfo
      then func t
      else notFound
