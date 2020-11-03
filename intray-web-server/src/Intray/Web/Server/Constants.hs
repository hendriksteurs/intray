module Intray.Web.Server.Constants where

import Import

development :: Bool
development =
  $( do
       md <- runIO $ lookupEnv "DEVELOPMENT"
       fmap ConE $ case md of
         Nothing -> pure 'False
         Just _ -> do
           runIO $ putStrLn "WARNING: BUILDING SMOS_WEB_SERVER IN DEVELOPMENT MODE"
           pure 'True
   )
