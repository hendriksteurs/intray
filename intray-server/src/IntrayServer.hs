module IntrayServer where

import Import
import Intray.Server (runIntrayServer)
import Intray.Server.OptParse (getSettings)

intrayServer :: IO ()
intrayServer = do
  settings <- getSettings
  putStrLn $ ppShow settings
  runIntrayServer settings
