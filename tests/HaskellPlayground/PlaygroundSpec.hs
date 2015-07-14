module HaskellPlayground.PlaygroundSpec (
    spec
) where

import Test.Hspec
import qualified Playground (result, parseLisp, LispValue(..))

spec :: Spec
spec = do
    describe "tokenizer" $ do
        it "should tokenize" $ do
            Playground.parseLisp "(foo bar (baz 42))" 
            `shouldBe` Right (Playground.List [
                Playground.Atom "foo", 
                Playground.Atom "bar", 
                Playground.List [Playground.Atom "baz", Playground.Number 42]])

