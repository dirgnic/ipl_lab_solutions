module ParsingTests where

import Syntax (
  Prg, Def(..), Exp(..))
import Parsing (
  parse, ParseError)

import Test.Tasty (
  TestTree, testGroup)
import Test.Tasty.HUnit (
  testCase, (@?=))


parsingTests :: TestTree
parsingTests = testGroup "parsing" [
  ]
  -- testCase "happy" (
  --   parse "def myMain() { let x = 4 + 8; let y = x + x; return y }" @?= Right [
  --     Fun "myMain" [] (
  --       Let "x" (Add (Num 4) (Num 8)) (
  --       Let "y" (Add (Var "x") (Var "x"))
  --       (Var "y")))])]


