module LoweringTests where

import Syntax (
  Stm(..), Exp(..))
import Lowering (
  lower)
import qualified IntermediateRepresentation as IR (
  JumpTarget(..), Block(Block), Binding(Let), App(..), Val(..), Jump(..))

import Test.Tasty (
  TestTree, testGroup)
import Test.Tasty.HUnit (
  testCase, (@?=))

-- Lowering tests
loweringTests :: TestTree
loweringTests = testGroup "lowering" [

  testCase "ten" (
    lower (Res (Num 10)) @?=
           [IR.JumpTarget "entrypoint" [] (IR.Block [] (IR.Jwa "return" [IR.Num 10])),
            IR.JumpTarget "return" ["result"] (IR.Block [] (IR.End (IR.Var "result")))]),

  testCase "shadowing" (
    lower (Let "x" (Num 5) (Let "x" (Num 100) (Res (Var "x")))) @?=
           [IR.JumpTarget "entrypoint" [] (IR.Block [] (IR.Jwa "l0" [IR.Num 5])),
            IR.JumpTarget "l1" ["x"] (IR.Block [] (IR.Jwa "l2" [IR.Num 100])),
            IR.JumpTarget "l2" ["x"] (IR.Block [] (IR.Jwa "return" [IR.Var "x"])),
            IR.JumpTarget "return" ["result"] (IR.Block [] (IR.End (IR.Var "result")))]),

  testCase "ifthenelse" (
    lower (Ite (Sma (Num 7) (Num 8)) (Res (Num 1)) (Res (Num 2))) @?=
           [IR.JumpTarget "entrypoint" [] (IR.Block [
              IR.Let "x0" (IR.Sma (IR.Num 7) (IR.Num 8))
              ] (IR.Bwa (IR.Var "x0") "l1" [] "l2" [])),
            IR.JumpTarget "return" ["result"] (IR.Block [] (IR.End (IR.Var "result"))),
            IR.JumpTarget "l1" [] (IR.Block [] (IR.Jwa "return" [IR.Num 1])),
            IR.JumpTarget "l2" [] (IR.Block [] (IR.Jwa "return" [IR.Num 2]))])]

