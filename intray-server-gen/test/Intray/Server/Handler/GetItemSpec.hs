{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Server.Handler.GetItemSpec
  ( spec,
  )
where

import Intray.API
import Intray.API.Gen ()
import Intray.Client
import Intray.Server.TestUtils
import TestImport

spec :: Spec
spec =
  withIntrayServer $
    describe "GetItem" $
      it "gets the same item that was just added" $
        \cenv ->
          forAllValid $ \t ->
            withValidNewUser cenv $ \token -> do
              i <-
                runClientOrError cenv $ do
                  uuid <- clientPostAddItem token t
                  clientGetItem token uuid
              itemInfoContents i `shouldBe` t
