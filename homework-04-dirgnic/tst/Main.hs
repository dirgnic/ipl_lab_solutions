module Main where

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
  typeCheckingTests,
  loweringTests,
  codeGenerationTests])


