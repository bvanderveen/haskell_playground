module HaskellPlayground.PlaygroundSpec (
    spec
) where

import Test.Hspec
import Playground.Parser
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

    describe "show" $ do

        it "should show atoms" $ do
            showValue (Atom "foo") `shouldBe` "foo"

        it "should show numbers" $ do
            showValue (Number 42) `shouldBe` "42"

        it "should show strings" $ do
            showValue (String "bar") `shouldBe` "\"bar\""

        it "should show booleans" $ do
            showValue (Bool False) `shouldBe` "false"
            showValue (Bool True) `shouldBe` "true"

        it "should show lists" $ do
            showValue (List [String "foo", Bool False, Number 42]) `shouldBe` "(\"foo\" false 42)"

        it "should show functions" $ do
            showValue (Function [("foo", (Atom "foo"))] ["arg1", "arg2"] [(List [Atom "bar", (Atom "arg1"), (Atom "arg2")])]) `shouldBe`
                "(lambda (arg1 arg2) (bar arg1 arg2))"

        it "should show function refs" $ do
            showValue (FunctionRef "+") `shouldBe` "+"

    describe "basic evaluation" $ do

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
            parseEval "'(true false)" `shouldBe` Right (List [Bool True, Bool False])

    describe "eval" $ do

        it "should evaluate the argument" $ do
            parseEval "(let (foo 'bar bar 'baz) (eval 'foo))" `shouldBe` Right (Atom "bar")

        it "should be evaluate the argument" $ do
            parseEval "(let (foo 'bar bar 'baz) (eval foo))" `shouldBe` Right (Atom "baz")

        it "should eval constants" $ do
            parseEval "(eval 5)" `shouldBe` Right (Number 5)

        it "should eval expressions" $ do            
            parseEval "(eval (+ 1 2))" `shouldBe` Right (Number 3)

        it "should eval forms in bindings" $ do            
            parseEval "(let (x '(+ 1 2)) (eval x))" `shouldBe` Right (Number 3)

        it "should eval quoted special forms" $ do
            parseEval "(eval '(+ 1 2 3))" `shouldBe` Right (Number 6)

        it "should eval quoted builtins" $ do
            parseEval "(eval '(if true 1 2))" `shouldBe` Right (Number 1)

    describe "def" $ do

        it "should add a binding to the environment" $ do 
            parseEvalInEnv nullEnv "(def foo \"bar\")" `shouldBe` Right ([("foo", (String "bar"))], (String "bar"))

        it "should evaluate to the evaluated second argument" $ do 
            parseEval "(def foo \"bar\")" `shouldBe` Right (String "bar")
            parseEval "(def foo (+ 2 4))" `shouldBe` Right (Number 6)
            parseEval "((lambda () (def foo \"bar\") foo))" `shouldBe` Right (String "bar")

    describe "lambda" $ do

        it "should eval lambdas" $ do
            parseEval "((lambda () \"foo\"))" `shouldBe` Right (String "foo")

        it "should eval builtins in lambdas" $ do
            parseEval "((lambda (arg1 arg2) (+ arg1 arg2 3)) 1 2)" `shouldBe` Right (Number 6)

    describe "if" $ do

        it "should eval if statements" $ do
            parseEval "(if false \"t\" \"f\")" `shouldBe` Right (String "f")
            parseEval "(if true 1 2)" `shouldBe` Right (Number 1)
            parseEval "(if (= 1 1) 1 2)" `shouldBe` Right (Number 1)

        it "should eval let" $ do
            parseEvalInEnv nullEnv "(let (a 42) a)" `shouldBe` Right ([], (Number 42))
            parseEval "(let (a 42 b 10) (+ a b))" `shouldBe` Right (Number 52)
            parseEval "(let (a (+ 40 3) b (* 2 5)) (+ a b))" `shouldBe` Right (Number 53)

        it "should eval recursive functions" $ do
            let factorial n = "((lambda () (def factorial (lambda (n) (if (= n 0) 1 (if (= n 1) 1 (* n (factorial (- n 1))))))) (factorial " ++ n ++ ")))"

            parseEval (factorial "0") `shouldBe` Right (Number 1)
            parseEval (factorial "1") `shouldBe` Right (Number 1)
            parseEval (factorial "2") `shouldBe` Right (Number 2)
            parseEval (factorial "3") `shouldBe` Right (Number 6)
            parseEval (factorial "4") `shouldBe` Right (Number 24)

    describe "builtins" $ do

        it "should eval addition and subtraction" $ do
            parseEval "(+ 1 2)" `shouldBe` Right (Number 3)
            parseEval "(- 1 2)" `shouldBe` Right (Number (-1))
            parseEval "(+ 1 2 3)" `shouldBe` Right (Number 6)
            parseEval "(+ 1 (+ 2 3) (+ 4 5) 5 (- 3 1))" `shouldBe` Right (Number 22)

        it "should eval multiply and divide" $ do
            parseEval "(* 3 4)" `shouldBe` Right (Number 12)
            parseEval "(/ 12 4)" `shouldBe` Right (Number 3)
            parseEval "(mod 12 5)" `shouldBe` Right (Number 2)

        it "should eval equality" $ do
            parseEval "(= 3 4)" `shouldBe` Right (Bool False)
            parseEval "(= 1 1)" `shouldBe` Right (Bool True)
            parseEval "(= '(12 5) '())" `shouldBe` Right (Bool False)
            parseEval "(= '(12 5 (false)) '(12 5 (false)))" `shouldBe` Right (Bool True)

        it "should map" $ do
            parseEval "(map (lambda (x) (+ x 1)) '(1 2 3))" `shouldBe` Right (List [Number 2, Number 3, Number 4])
            parseEval "(let (f (lambda (x) (+ x 1))) (map f '(1 2 3)))" `shouldBe` Right (List [Number 2, Number 3, Number 4])

        it "should reduce" $ do
            parseEval "(reduce + 0 '(1 2 3))" `shouldBe` Right (Number 6)
            parseEval "(let (f +) (reduce f 0 '(1 2 3)))" `shouldBe` Right (Number 6)
            parseEval "(reduce (lambda (a i) (+ a i)) 0 '(1 2 3))" `shouldBe` Right (Number 6)
            parseEval "(let (f (lambda (a i) (+ a i))) (reduce f 0 '(1 2 3)))" `shouldBe` Right (Number 6)
