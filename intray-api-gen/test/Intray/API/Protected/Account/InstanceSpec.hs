{-# LANGUAGE TypeApplications #-}

module Intray.API.Protected.Account.InstanceSpec
  ( spec,
  )
where

import Intray.API.Protected.Account.Gen ()
import Intray.API.Protected.Account.Types
import Test.Validity.Aeson
import TestImport

spec :: Spec
spec = do
  genValidSpec @AccountInfo
  jsonSpecOnValid @AccountInfo
  genValidSpec @ChangePassphrase
  jsonSpecOnValid @ChangePassphrase
