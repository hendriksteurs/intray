{-# LANGUAGE TypeApplications #-}

module Intray.API.InstanceSpec
  ( spec,
  )
where

import Intray.API.Gen ()
import Intray.API.Types
import Test.Validity.Aeson
import TestImport

spec :: Spec
spec = do
  genValidSpec @Registration
  jsonSpecOnValid @Registration
  genValidSpec @LoginForm
  jsonSpecOnValid @LoginForm
  genValidSpec @Pricing
  jsonSpecOnValid @Pricing
