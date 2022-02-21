{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Intray.Web.Server.Handler.Admin
  ( getAdminPanelR,
    getAdminAccountR,
    postAdminAccountDeleteR,
    postAdminAccountSetSubscriptionR,
  )
where

import Control.Exception (throwIO)
import Data.Time
import Import
import Intray.Client
import Intray.Web.Server.Foundation
import Intray.Web.Server.Time
import Network.HTTP.Types as HTTP
import Text.Time.Pretty
import Yesod

getAdminPanelR :: Handler Html
getAdminPanelR =
  withAdminCreds $ \t -> do
    AdminStats {..} <- runClientOrErr $ clientAdminGetStats t
    mPricing <- runClientOrErr clientGetPricing
    let ActiveUsers {..} = adminStatsActiveUsers
    users <- runClientOrErr $ clientAdminGetAccounts t
    now <- liftIO getCurrentTime
    token <- genToken
    withNavBar $(widgetFile "admin/panel")

getAdminAccountR :: Username -> Handler Html
getAdminAccountR username = withAdminCreds $ \t -> do
  errOrAccountInfo <- runClient $ clientAdminGetAccount t username
  case errOrAccountInfo of
    Left err -> case err of
      FailureResponse _ resp ->
        if HTTP.statusCode (responseStatusCode resp) == 404
          then notFound
          else liftIO $ throwIO err
      _ -> liftIO $ throwIO err
    Right AccountInfo {..} -> do
      now <- liftIO getCurrentTime
      withNavBar $ do
        token <- genToken
        $(widgetFile "admin/account")

postAdminAccountDeleteR :: Username -> Handler Html
postAdminAccountDeleteR uuid =
  withAdminCreds $ \t -> do
    NoContent <- runClientOrErr $ clientAdminDeleteAccount t uuid
    redirect $ AdminR AdminPanelR

postAdminAccountSetSubscriptionR :: Username -> Handler Html
postAdminAccountSetSubscriptionR username = withAdminCreds $ \t -> do
  endDate <- runInputPost $ ireq dayField "end-date"
  NoContent <- runClientOrErr $ clientAdminPutAccountSubscription t username $ UTCTime endDate 0
  redirect $ AdminR $ AdminAccountR username

withAdminCreds :: (Token -> Handler Html) -> Handler Html
withAdminCreds func =
  withLogin $ \t -> do
    adminInfo <- runClientOrErr $ clientGetAccountInfo t
    if accountInfoAdmin adminInfo
      then func t
      else notFound
