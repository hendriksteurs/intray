module Intray.CliSpec
  ( spec,
  )
where

import Intray.Cli.TestUtils
import Intray.Server.TestUtils
import TestImport

spec :: Spec
spec = sequential $
  withIntrayServer $
    do
      it "Going through the usual manual steps 'just works'" $ \(ClientEnv _ burl _) ->
        withSystemTempDir "intray-cli-test-cache" $ \cacheDir ->
          withSystemTempDir "intray-cli-test-data" $ \dataDir -> do
            setEnv "INTRAY_USERNAME" "testuser"
            setEnv "INTRAY_PASSWORD" "testpassword"
            setEnv "INTRAY_URL" $ showBaseUrl burl
            setEnv "INTRAY_CACHE_DIR" $ fromAbsDir cacheDir
            setEnv "INTRAY_DATA_DIR" $ fromAbsDir dataDir
            intray ["register"]
            intray ["login"]
            intray ["add", "hello", "world"]
            intray ["show"]
            intray ["done"]
            intray ["size"]
            intray ["sync"]
            intray ["logout"]
      it "Going through the usual manual steps 'just works' with a nonexistent data dir" $ \(ClientEnv _ burl _) ->
        forAllValid $ \relDataDir ->
          withSystemTempDir "intray-cli-test-cache" $ \cacheDir ->
            withSystemTempDir "intray-cli-test-data" $ \dataDir -> do
              setEnv "INTRAY_USERNAME" "testuser"
              setEnv "INTRAY_PASSWORD" "testpassword"
              setEnv "INTRAY_URL" $ showBaseUrl burl
              setEnv "INTRAY_CACHE_DIR" $ fromAbsDir cacheDir
              setEnv "INTRAY_DATA_DIR" $ fromAbsDir $ dataDir </> relDataDir
              intray ["register"]
              intray ["login"]
              intray ["add", "hello", "world"]
              intray ["show"]
              intray ["done"]
              intray ["size"]
              intray ["sync"]
              intray ["logout"]
      it "Going through the usual manual steps 'just works' with a nonexistent cache dir" $ \(ClientEnv _ burl _) ->
        forAllValid $ \relCacheDir ->
          withSystemTempDir "intray-cli-test-cache" $ \cacheDir ->
            withSystemTempDir "intray-cli-test-data" $ \dataDir -> do
              setEnv "INTRAY_USERNAME" "testuser"
              setEnv "INTRAY_PASSWORD" "testpassword"
              setEnv "INTRAY_URL" $ showBaseUrl burl
              setEnv "INTRAY_CACHE_DIR" $ fromAbsDir $ cacheDir </> relCacheDir
              setEnv "INTRAY_DATA_DIR" $ fromAbsDir dataDir
              intray ["register"]
              intray ["login"]
              intray ["add", "hello", "world"]
              intray ["show"]
              intray ["done"]
              intray ["size"]
              intray ["sync"]
              intray ["logout"]
