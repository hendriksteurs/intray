{-# LANGUAGE RecordWildCards #-}

module Intray.Web.Server
  ( intrayWebServer,
  )
where

import Control.Monad.Logger
import qualified Data.Text as T
import Database.Persist.Sqlite
import Import
import Intray.Web.Server.Application ()
import Intray.Web.Server.Foundation
import Intray.Web.Server.OptParse
import qualified Network.HTTP.Client as Http
import Yesod

intrayWebServer :: IO ()
intrayWebServer = do
  (DispatchServe ss, Settings) <- getInstructions
  pPrint ss
  runIntrayWebServer ss

runIntrayWebServer :: ServeSettings -> IO ()
runIntrayWebServer ServeSettings {..} =
  runStderrLoggingT $
    filterLogger (\_ ll -> ll >= serveSetLogLevel) $
      withSqlitePoolInfo (mkSqliteConnectionInfo $ T.pack serveSetLoginCacheFile) 1 $
        \pool -> do
          man <- liftIO $ Http.newManager Http.defaultManagerSettings
          let app =
                App
                  { appHttpManager = man,
                    appStatic = myStatic,
                    appTracking = serveSetTracking,
                    appVerification = serveSetVerification,
                    appAPIBaseUrl = serveSetAPIBaseUrl,
                    appConnectionPool = pool
                  }
          liftIO $ do
            runSqlPool (runMigration migrateLoginCache) pool
            warp serveSetPort app
