module HaskellPlayground.PlaygroundSpec (
    spec
) where

import Test.Hspec
import Playground

spec :: Spec
spec = do
    describe "tokenizer" $ do
        it "should tokenize" $ do
            parseLisp "(foo bar (baz 42) ())" 
            `shouldBe` Right (List [
                Atom "foo", 
                Atom "bar", 
                List [Atom "baz", Number 42],
                List []])

    describe "eval" $ do
        it "should eval numbers" $ do
            parseEval "42" `shouldBe` Right (Number 42)
            
        it "should eval strings" $ do
            parseEval "\"foo\"" `shouldBe` Right (String "foo")

        it "should eval booleans" $ do
            parseEval "true" `shouldBe` Right (Bool True)
            parseEval "false" `shouldBe` Right (Bool False)

        it "should eval quotes" $ do
            parseEval "(quote foo)" `shouldBe` Right (Atom "foo")
            parseEval "(quote ())" `shouldBe` Right (List [])
            parseEval "(quote (true false))" `shouldBe` Right (List [Bool True, Bool False])



