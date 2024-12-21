module ParsingTests where

import Parsing (
  parse_func)

import Text.Megaparsec (errorBundlePretty)

-- Ass
import Test.Tasty.Golden (
  goldenVsFileDiff)
import Test.Tasty
import Test.Tasty.HUnit


parsingTests :: TestTree
parsingTests = testGroup "parsing" [
  testParsing "ten",
  testParsing "let_test",
  testParsing "add_test",
  testParsing "if_test",
  testParsing "and_test",
  testParsing "complex_test"]

normalizeOutput :: String -> String
normalizeOutput output =
  if last output == '\n'
    then output
    else output ++ "\n"

-- | Test code generation for the given program. The test will have the given name.
testParsing :: String -> TestTree
testParsing name =
  goldenVsFileDiff name diffCommand (goldenName name) (outputName name) (do
    i <- readFile (inputName name)
    case parse_func i of
      Right s -> writeFile (outputName name) (normalizeOutput (show s))
      Left e -> writeFile (outputName name) (normalizeOutput (errorBundlePretty e)))



inputName :: String -> FilePath
inputName name = "test-data/parsing/" ++ name ++ ".input"

goldenName :: String -> FilePath
goldenName name = "test-data/parsing/" ++ name ++ ".golden"

outputName :: String -> FilePath
outputName name = "test-data/parsing/" ++ name ++ ".output"

diffCommand :: String -> String -> [String]
diffCommand ref new = ["diff", "-u", ref, new]
