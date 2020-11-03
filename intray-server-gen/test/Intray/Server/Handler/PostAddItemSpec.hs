{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Server.Handler.PostAddItemSpec
  ( spec,
  )
where

import Intray.API.Gen ()
import Intray.Client
import Intray.Server.TestUtils
import TestImport

spec :: Spec
spec =
  withIntrayServer
    $ describe "PostAddItem"
    $ do
      it "fails without PermitAdd" $ \cenv ->
        forAllValid $ \item -> failsWithOutPermission cenv PermitAdd $ \t -> clientPostAddItem t item
      it "adds an item without crashing" $ \cenv ->
        forAllValid $ \t ->
          withValidNewUser cenv $ \token -> do
            uuid <- runClientOrError cenv $ clientPostAddItem token t
            shouldBeValid uuid
