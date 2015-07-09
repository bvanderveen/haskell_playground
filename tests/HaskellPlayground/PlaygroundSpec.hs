module HaskellPlayground.PlaygroundSpec (
    spec
) where

import Test.Hspec
import Playground

spec :: Spec
spec = do
    describe "Playground" $ do
        it "should foo" $ do
            True `shouldBe` True
        it "should filter" $ do
            Playground.result 4 `shouldBe` [1, 2, 3]
