{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.Commands.Login
  ( login,
  )
where

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Import
import Intray.API
import Intray.Cli.Client
import Intray.Cli.OptParse
import Intray.Cli.Prompt
import Intray.Cli.Session
import Intray.Client
import Web.Cookie (parseSetCookie, setCookieName)

login :: LoginSettings -> CliM ()
login LoginSettings {..} = do
  sets <- ask
  mRes <-
    runSingleClientOrErr $ do
      loginForm <-
        liftIO $
          runReaderT
            (LoginForm <$> promptUsername loginSetUsername <*> promptPassword loginSetPassword)
            sets
      clientPostLogin loginForm
  case mRes of
    Nothing -> liftIO $ die "No server configured."
    Just (Headers NoContent (HCons sessionHeader HNil)) ->
      case sessionHeader of
        MissingHeader ->
          liftIO $ die "The server responded but the response was missing the right session header."
        UndecodableHeader _ ->
          liftIO $ die "The server responded but the response had an undecodable session header."
        Header setCookieText -> do
          let cookies = parseSetCookie . encodeUtf8 <$> T.lines setCookieText
              jwtCookie = find ((== "JWT-Cookie") . setCookieName) cookies
          case jwtCookie of
            Nothing -> liftIO $ die "No JWT-Cookie was found in the Set-Cookie session header."
            Just setCookie -> saveSession setCookie
