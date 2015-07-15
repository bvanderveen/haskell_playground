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

        it "should eval addition and subtraction" $ do
            parseEval "(+ 1 2)" `shouldBe` Right (Number 3)
            parseEval "(- 1 2)" `shouldBe` Right (Number (-1))
            parseEval "(+ 1 2 3)" `shouldBe` Right (Number 6)
            parseEval "(+ 1 (+ 2 3) (+ 4 5) 5 (- 3 1))" `shouldBe` Right (Number 22)

        it "should eval multiply and divide" $ do
            parseEval "(* 3 4)" `shouldBe` Right (Number 12)
            parseEval "(/ 12 4)" `shouldBe` Right (Number 3)
            parseEval "(mod 12 5)" `shouldBe` Right (Number 2)


