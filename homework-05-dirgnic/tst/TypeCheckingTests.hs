module TypeCheckingTests where

import Syntax (
  Stm(..), Exp(..))
import qualified TypeChecking as TC  -- Correct import

import Test.Tasty (
  TestTree, testGroup)
import Test.Tasty.HUnit (
  testCase, (@?=))

typeCheckingTests :: TestTree
typeCheckingTests = testGroup "typeChecking" [

  -- Corrected test cases:
  testCase "addFiveFalse" (TC.typeCheck (Res (Add (Num 5) (Boo False))) @?= Left TC.TypeMismatch),
  testCase "unboundVariable" (TC.typeCheck (Res (Add (Num 9) (Var "x"))) @?= Left TC.NotInScope),
  testCase "ifthenelse" (TC.typeCheck (Ite (Sma (Num 4) (Num 9)) (Res (Num 8)) (Res (Num 14))) @?= Right TC.TypeInt)
 ]
