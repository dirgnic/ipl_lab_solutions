module Main where

import ParsingTests (
  parsingTests)
import TypeCheckingTests (
  typeCheckingTests)
import LoweringTests (
  loweringTests)
import CodeGenerationTests (
  codeGenerationTests)

import Test.Tasty (
  defaultMain, testGroup)


main :: IO ()
main = defaultMain (testGroup "tests" [
  parsingTests,
  typeCheckingTests,
  loweringTests,
  codeGenerationTests])


