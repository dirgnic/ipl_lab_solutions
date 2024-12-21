module TypeCheckingTests where

import Syntax (
  Stm(..), Exp(..))
import TypeChecking (
  typeCheck, TypeError(..))

import Test.Tasty (
  TestTree, testGroup)
import Test.Tasty.HUnit (
  testCase, (@?=))


typeCheckingTests :: TestTree
typeCheckingTests = testGroup "typeChecking" [

  testCase "ten" (
    typeCheck (Res (Num 10)) @?= Nothing),

  testCase "addFiveFalse" (
    typeCheck (Res (Add (Num 5) (Boo False))) @?= Just TypeMismatch),

  testCase "unboundReference" (
    typeCheck (Res (Add (Num 9) (Get "x"))) @?= Just NotInScope),

  testCase "ifthenelse" (
    typeCheck (
      Dcl "x" (
      Ite (Sma (Num 4) (Num 9)) (Res (Num 8)) (Res (Num 14)) (
      Res (Get "x")))) @?= Nothing)]

