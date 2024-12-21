module LoweringTests where

import Syntax (
  Stm(..), Exp(..))
import Lowering (
  lower)
import qualified IntermediateRepresentation as IR (
  JoinPoint(..), Block(Block), Stm(..), App(..), Val(..), Jump(..))

import Test.Tasty (
  TestTree, testGroup)
import Test.Tasty.HUnit (
  testCase, (@?=))


loweringTests :: TestTree
loweringTests = testGroup "lowering" [

  testCase "ten" (
    lower (Res (Num 10)) @?=
           [IR.JoinPoint "entrypoint" (IR.Block [
              ] (IR.End (IR.Num 10))),
            IR.JoinPoint "return" (IR.Block [
              ] (IR.End (IR.Num 0)))]),

  testCase "setting_twice" (
    lower (Dcl "x" (
           Set "x" (Num 5) (
           Set "x" (Num 100) (
           Res (Get "x"))))) @?=
           [IR.JoinPoint "entrypoint" (IR.Block [
              IR.Let "x0" (IR.Alo "x"),
              IR.Let "x1" (IR.Sto "x" (IR.Num 5)),
              IR.Let "x2" (IR.Sto "x" (IR.Num 100)),
              IR.Let "x3" (IR.Loa "x")]
              (IR.End (IR.Var "x3"))),
            IR.JoinPoint "return" (IR.Block [
              ] (IR.End (IR.Num 0)))]),

  testCase "ifthenelse" (
    lower (Dcl "r" (
           Ite (Sma (Num 7) (Num 8)) (Set "r" (Num 1) Ski) (Set "r" (Num 2) Ski) (
           Res (Get "x")))) @?=
           [IR.JoinPoint "entrypoint" (IR.Block [
              IR.Let "x0" (IR.Alo "r"),
              IR.Let "x4" (IR.Sma (IR.Num 7) (IR.Num 8))
              ] (IR.Bra (IR.Var "x4") "l1" "l2")),
            IR.JoinPoint "l1" (IR.Block [
              IR.Let "x5" (IR.Sto "r" (IR.Num 1))
              ] (IR.Jum "l3")),
            IR.JoinPoint "l2" (IR.Block [
              IR.Let "x6" (IR.Sto "r" (IR.Num 2))
              ] (IR.Jum "l3")),
            IR.JoinPoint "l3" (IR.Block [
              IR.Let "x7" (IR.Loa "x")
              ] (IR.End (IR.Var "x7"))),
            IR.JoinPoint "return" (IR.Block [
              ] (IR.End (IR.Num 0)))])]
