module ParsingTests where

import Syntax (
  Stm(..), Exp(..))
import Parsing (
  parse)

import Test.Tasty (
  TestTree, testGroup)
import Test.Tasty.HUnit (
  testCase, (@?=))


parsingTests :: TestTree
parsingTests = testGroup "parsing" [
  testCase "happy" (
    parse "int x; x = 4 + 8; int y; y = x + x; return y" @?= Right (
      Dcl "x" (
      Set "x" (Add (Num 4) (Num 8)) (
      Dcl "y" (
      Set "y" (Add (Get "x") (Get "x")) (
      Res (Get "y"))))))),
  testCase "multiple references" (
    parse "int x; x = 5; int x; x = 9;" @?= Right (
      Dcl "x" (
      Set "x" (Num 5) (
      Dcl "x" (
      Set "x" (Num 9)
      Ski))))),
  testCase "while" (
    parse "int x; x = 0; while ( x < 10 ) { x = x + 1 ; skip }; return x" @?= Right (
      Dcl "x" (
      Set "x" (Num 0) (
      Whi (Sma (Get "x") (Num 10)) (Set "x" (Add (Get "x") (Num 1)) Ski) (
      Res (Get "x")))))),
  testCase "new" (
    parse "new int x; x = 0; return x" @?= Right (
      New "x" (
      Set "x" (Num 0) (
      Res (Get "x"))))),
  testCase "parse error" (
    parse "int x = 9" @?= Left (
      "myModule:1:7:\n  |\n1 | int x = 9\n  |       ^\nunexpected '='\nexpecting ';' or white space\n"))]

