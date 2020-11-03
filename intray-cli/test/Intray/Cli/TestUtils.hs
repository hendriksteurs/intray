module Intray.Cli.TestUtils
  ( intray,
  )
where

import Intray.Cli (intrayCli)
import TestImport

intray :: [String] -> IO ()
intray args = do
  putStrLn $ unwords $ "RUNNING:" : "intray" : args
  withArgs args intrayCli -- Not threadsafe
