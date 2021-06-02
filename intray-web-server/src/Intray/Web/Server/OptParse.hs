{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Intray.Web.Server.OptParse
  ( getInstructions,
    Instructions,
    Dispatch (..),
    Settings (..),
    ServeSettings (..),
  )
where

import Control.Arrow
import Control.Monad.Logger
import qualified Data.Text as T
import qualified Env
import Import
import Intray.Web.Server.OptParse.Types
import Options.Applicative
import qualified Options.Applicative.Help as OptParse
import Servant.Client
import qualified System.Environment as System
import qualified YamlParse.Applicative as YamlParse

getInstructions :: IO Instructions
getInstructions = do
  (cmd, flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions cmd flags env config

combineToInstructions :: Command -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (CommandServe ServeFlags {..}) Flags {..} Environment {..} mConf = do
  let mc :: (Configuration -> Maybe a) -> Maybe a
      mc func = mConf >>= func
  let port = fromMaybe 8000 $ serveFlagPort <|> envPort <|> mc confPort
  baseUrl <- case serveFlagAPIBaseUrl <|> envAPIBaseUrl <|> mc confAPIBaseUrl of
    Nothing -> die "No API URL Configured. Try --help to see how to configure it."
    Just burl -> pure burl
  pure
    ( DispatchServe
        ServeSettings
          { serveSetPort = port,
            serveSetLogLevel = fromMaybe LevelInfo $ serveFlagLogLevel <|> envLogLevel <|> mc confLogLevel,
            serveSetAPIBaseUrl = baseUrl,
            serveSetTracking = serveFlagTracking <|> envTracking <|> mc confTracking,
            serveSetVerification = serveFlagVerification <|> envVerification <|> mc confVerification,
            serveSetLoginCacheFile =
              fromMaybe "intray-web-server.db" $
                serveFlagLoginCacheFile <|> envLoginCacheFile <|> mc confLoginCacheFile
          },
      Settings
    )

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} = do
  configFile <- case flagConfigFile <|> envConfigFile of
    Nothing -> getDefaultConfigFile
    Just cf -> resolveFile' cf
  YamlParse.readConfigFile configFile

getDefaultConfigFile :: IO (Path Abs File)
getDefaultConfigFile = resolveFile' "config.yaml"

getEnvironment :: IO Environment
getEnvironment = Env.parse id environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "INTRAY_WEB_SERVER_" $
    Environment
      <$> Env.var (fmap Just . Env.str) "CONFIG_FILE" (mE "Config file")
      <*> Env.var (fmap Just . Env.auto) "PORT" (mE "port to run the web server on")
      <*> Env.var (fmap Just . Env.auto) "LOG_LEVEL" (mE "minimal severity for log messages")
      <*> Env.var (fmap Just . left (Env.UnreadError . show) . parseBaseUrl) "API_URL" (mE "base url for the api server to call")
      <*> Env.var (fmap Just . Env.str) "ANALYTICS_TRACKING_ID" (mE "google analytics tracking id")
      <*> Env.var
        (fmap Just . Env.str)
        "SEARCH_CONSOLE_VERIFICATION"
        (mE "google search console verification id")
      <*> Env.var (fmap Just . Env.str) "LOGIN_CACHE_FILE" (mE "google search console verification id")
  where
    mE h = Env.def Nothing <> Env.keep <> Env.help h

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ = defaultPrefs {prefShowHelpOnError = True, prefShowHelpOnEmpty = True}

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) (fullDesc <> footerDoc (Just $ OptParse.string footerStr))
  where
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (YamlParse.prettyColourisedSchemaDoc @Configuration)
        ]

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat [command "serve" parseCommandServe]

parseCommandServe :: ParserInfo Command
parseCommandServe = info parser modifier
  where
    parser =
      CommandServe
        <$> ( ServeFlags
                <$> optional
                  ( option
                      auto
                      ( mconcat
                          [ long "port",
                            metavar "PORT",
                            help "the port to serve on"
                          ]
                      )
                  )
                <*> optional
                  ( option
                      (eitherReader $ left show . parseBaseUrl)
                      ( mconcat
                          [ long "api-url",
                            metavar "URL",
                            help "the url to call the API server on"
                          ]
                      )
                  )
                <*> optional
                  ( option
                      auto
                      ( mconcat
                          [ long "log-level",
                            metavar "LOG_LEVEL",
                            help "the minimal severity for log messages"
                          ]
                      )
                  )
                <*> optional
                  ( strOption
                      ( mconcat
                          [ long "analytics-tracking-id",
                            metavar "TRACKING_ID",
                            help "The google analytics tracking ID"
                          ]
                      )
                  )
                <*> optional
                  ( strOption
                      ( mconcat
                          [ long "search-console-verification",
                            metavar "VERIFICATION_TAG",
                            help "The contents of the google search console verification tag"
                          ]
                      )
                  )
                <*> optional
                  ( strOption
                      ( mconcat
                          [ long "login-cache-file",
                            metavar "FILEPATH",
                            help "The file to store the login cache database in"
                          ]
                      )
                  )
            )
    modifier = fullDesc <> progDesc "Serve requests" <> YamlParse.confDesc @Configuration

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> option
      (Just <$> str)
      (mconcat [long "config-file", value Nothing, metavar "FILEPATH", help "The config file"])
