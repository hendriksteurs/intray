{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Criterion.Main
import qualified Data.ByteString as SB
import Intray.Client
import Intray.Server.TestUtils
import Network.HTTP.Client as HTTP
import Test.Syd.Def.SetupFunc

smallTextItem :: TypedItem
smallTextItem = TypedItem {itemType = TextItem, itemData = "Example Data"}

largeTextItem :: TypedItem
largeTextItem = smallTextItem {itemData = SB.concat $ replicate 100 $ itemData smallTextItem}

main :: IO ()
main = do
  man <- newManager defaultManagerSettings
  -- TODO empty the server for each benchmark, otherwise benchmarks are not independent
  withServer man $ \cenv -> do
    (_, tok) <- setupTestUser cenv
    defaultMain
      [ bench "register" $ register cenv,
        bench "register and login" $ registerAndLogin cenv,
        bench "size" $ size cenv tok,
        bench "add small text item" $ add cenv tok smallTextItem,
        bench "add large text item" $ add cenv tok largeTextItem,
        bench "add and get small text item" $ addAndGet cenv tok smallTextItem,
        bench "add and get large text item" $ addAndGet cenv tok smallTextItem,
        bench "add and delete small text item" $ addAndDelete cenv tok smallTextItem,
        bench "add and delete large text item" $ addAndDelete cenv tok largeTextItem
      ]

register :: ClientEnv -> Benchmarkable
register cenv =
  whnfIO $ do
    r <- randomRegistration
    runClientOrError cenv $ clientPostRegister r

registerAndLogin :: ClientEnv -> Benchmarkable
registerAndLogin cenv =
  whnfIO $ do
    r <- randomRegistration
    runClientOrError cenv $ do
      NoContent <- clientPostRegister r
      clientPostLogin $ registrationLoginForm r

size :: ClientEnv -> Token -> Benchmarkable
size cenv tok = whnfIO $ runClientOrError cenv $ clientGetSize tok

add :: ClientEnv -> Token -> TypedItem -> Benchmarkable
add cenv tok ti = whnfIO $ runClientOrError cenv $ clientPostAddItem tok ti

addAndGet :: ClientEnv -> Token -> TypedItem -> Benchmarkable
addAndGet cenv tok ti =
  whnfIO $
    runClientOrError cenv $
      do
        u <- clientPostAddItem tok ti
        clientGetItem tok u

addAndDelete :: ClientEnv -> Token -> TypedItem -> Benchmarkable
addAndDelete cenv tok ti =
  whnfIO $
    runClientOrError cenv $
      do
        u <- clientPostAddItem tok ti
        clientDeleteItem tok u

setupTestUser :: ClientEnv -> IO (Registration, Token)
setupTestUser cenv = do
  r <- randomRegistration
  NoContent <- runClientOrError cenv $ clientPostRegister r
  t <- login cenv (registrationUsername r) (registrationPassword r)
  pure (r, t)

registrationLoginForm :: Registration -> LoginForm
registrationLoginForm Registration {..} =
  LoginForm {loginFormUsername = registrationUsername, loginFormPassword = registrationPassword}

withServer :: HTTP.Manager -> (ClientEnv -> IO ()) -> IO ()
withServer man = unSetupFunc $ intrayTestClientEnvSetupFunc Nothing man
