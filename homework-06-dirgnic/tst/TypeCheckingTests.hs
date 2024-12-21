module TypeCheckingTests where

import Syntax (
  Def(..), Exp(..), Type(..))
import TypeChecking (
  typeCheck, TypeError(..))

import Test.Tasty (
  TestTree, testGroup)
import Test.Tasty.HUnit (
  testCase, (@?=))


typeCheckingTests :: TestTree
typeCheckingTests = testGroup "typeChecking" [

  testCase "ten" (
    typeCheck [Fun "myMain" [] TypeInt (Num 10)] @?= Nothing),

  testCase "addFiveFalse" (
    typeCheck [Fun "myMain" [] TypeInt (Add (Num 5) (Boo False))] @?= Just TypeMismatch),

  testCase "unboundVariable" (
    typeCheck [Fun "myMain" [] TypeInt (Add (Num 9) (Var "x"))] @?= Just NotInScope),

  testCase "ifthenelse" (
    typeCheck [Fun "myMain" [] TypeInt (
      Ite (Sma (Num 4) (Num 9)) (Num 8) (Num 14))] @?= Nothing),
  testCase "functionDefinition" (
    typeCheck [Fun "add" [("x", TypeInt), ("y", TypeInt)] TypeInt (Add (Var "x") (Var "y"))] @?= Nothing),

  testCase "functionCallMismatch" (
    typeCheck [Fun "add" [("x", TypeInt), ("y", TypeInt)] TypeInt (Add (Var "x") (Var "y")),
              Fun "main" [] TypeInt (App "add" [Boo True, Num 5])] @?= Just TypeMismatch)]

