{-# LANGUAGE TypeApplications #-}

module Intray.Data.InstanceSpec
  ( spec
  ) where

import Intray.Data
import Intray.Data.Gen ()
import Test.Syd.Validity.Aeson
import TestImport

spec :: Spec
spec = do
  jsonSpec @ItemUUID
  genValidSpec @ItemUUID
  jsonSpec @ItemType
  genValidSpec @ItemType
  genValidSpec @IntrayItem
  genValidSpec @Username
  jsonSpec @Username
  eqSpec @HashedPassword
  genValidSpec @HashedPassword
  genValidSpec @AccountUUID
  jsonSpec @AccountUUID
  genValidSpec @User
  genValidSpec @Permission
  jsonSpec @Permission
  genValidSpec @AccessKeySecret
  jsonSpec @AccessKeySecret
