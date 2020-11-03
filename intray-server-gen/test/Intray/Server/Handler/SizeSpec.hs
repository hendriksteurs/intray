module Intray.Server.Handler.SizeSpec
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
    $ describe "GetSize"
    $ do
      it "fails without PermitSize" $ \cenv -> failsWithOutPermission cenv PermitSize clientGetSize
      it "does not count other accounts' items" $ \cenv ->
        forAllValid $ \t ->
          withValidNewUser cenv $ \t1 ->
            withValidNewUser cenv $ \t2 -> do
              mr <-
                runClientOrError cenv $ do
                  void $ clientPostAddItem t1 t
                  clientGetSize t2
              mr `shouldBe` 0
