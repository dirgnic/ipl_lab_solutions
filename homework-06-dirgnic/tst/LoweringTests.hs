module LoweringTests where

import qualified Syntax as S (
  Type(..), Def(..), Exp(..))
import Lowering (
  lower)
import IntermediateRepresentation (
  Function(..), JumpTarget(..), Block(..), Binding(..), App(..), Val(..), Jump(..))

import Test.Tasty (
  TestTree, testGroup)
import Test.Tasty.HUnit (
  testCase, (@?=))


loweringTests :: TestTree
loweringTests = testGroup "lowering" [

  testCase "ten" (
    lower [S.Fun "myMain" [] S.TypeInt
              (S.Num 10)] @?=

           [Function "myMain" [] S.TypeInt [
            JumpTarget "entrypoint" [] (Block [
              ] (Jwa "return" [Num 10])),
            JumpTarget "return" ["result"] (Block [
              ] (End (Var "result")))]]),

  testCase "comparison" (
    lower [S.Fun "myMain" [] S.TypeInt
              (S.Sma (S.Num 15) (S.Num 8))] @?=

          [Function "myMain" [] S.TypeInt [JumpTarget "entrypoint" [] (Block [] (Jwa "l2" [Num 15])),JumpTarget "l4" ["x3"] (Block [] (Jwa "l0" [])),JumpTarget "l2" ["x1"] (Block [] (Jwa "l4" [Num 8])),JumpTarget "l0" [] (Block [Let "x5" (Sma (Var "x1") (Var "x3"))] (Jwa "return" [Var "x5"])),JumpTarget "return" ["result"] (Block [] (End (Var "result")))]]),

  testCase "ifthenelse" (
    lower [S.Fun "myMain" [] S.TypeInt
              (S.Ite (S.Sma (S.Num 7) (S.Num 8))
                 (S.Num 1)
                 (S.Num 2))] @?=

          [Function "myMain" [] S.TypeInt [JumpTarget "entrypoint" [] (Block [] (Jwa "l4" [Num 7])),JumpTarget "l6" ["x5"] (Block [] (Jwa "l2" [])),JumpTarget "l4" ["x3"] (Block [] (Jwa "l6" [Num 8])),JumpTarget "l2" [] (Block [Let "x7" (Sma (Var "x3") (Var "x5"))] (Bwa (Var "x7") "l0" [] "l1" [])),JumpTarget "l0" [] (Block [] (Jwa "return" [Num 1])),JumpTarget "l1" [] (Block [] (Jwa "return" [Num 2])),JumpTarget "return" ["result"] (Block [] (End (Var "result")))]]),

  testCase "function" (
    lower [
      S.Fun "myMain" [] S.TypeInt (S.App "f" [S.Num 10]),
      S.Fun "f" [("x", S.TypeInt)] S.TypeInt (S.Var "x")] @?=

          [Function "myMain" [] S.TypeInt [JumpTarget "entrypoint" [] (Block [] (Jwa "l2" [Num 10])),JumpTarget "l2" ["x1"] (Block [] (Jwa "l0" [])),JumpTarget "l0" [] (Block [Let "x3" (Cal "f" [Var "x1"])] (Jwa "return" [Var "x3"])),JumpTarget "return" ["result"] (Block [] (End (Var "result")))],Function "f" [("x",S.TypeInt)] S.TypeInt [JumpTarget "entrypoint" [] (Block [] (Jwa "return" [Var "x"])),JumpTarget "return" ["result"] (Block [] (End (Var "result")))]]),

  testCase "addNums" (
    lower [S.Fun "myMain" [] S.TypeInt (
              S.App "addNums" [S.Num 10]),
           S.Fun "addNums" [("x", S.TypeInt)] S.TypeInt (
             S.Ite (S.Sma (S.Var "x") (S.Num 1))
               (S.Num 0)
               (S.Add (S.Var "x") (S.App "addNums" [S.Sub (S.Var "x") (S.Num 1)])))] @?=

          [Function "myMain" [] S.TypeInt [JumpTarget "entrypoint" [] (Block [] (Jwa "l2" [Num 10])),JumpTarget "l2" ["x1"] (Block [] (Jwa "l0" [])),JumpTarget "l0" [] (Block [Let "x3" (Cal "addNums" [Var "x1"])] (Jwa "return" [Var "x3"])),JumpTarget "return" ["result"] (Block [] (End (Var "result")))],Function "addNums" [("x",S.TypeInt)] S.TypeInt [JumpTarget "entrypoint" [] (Block [] (Jwa "l4" [Var "x"])),JumpTarget "l6" ["x5"] (Block [] (Jwa "l2" [])),JumpTarget "l4" ["x3"] (Block [] (Jwa "l6" [Num 1])),JumpTarget "l2" [] (Block [Let "x7" (Sma (Var "x3") (Var "x5"))] (Bwa (Var "x7") "l0" [] "l1" [])),JumpTarget "l12" ["x11"] (Block [] (Jwa "l8" [])),JumpTarget "l15" ["x14"] (Block [] (Jwa "l13" [])),JumpTarget "l20" ["x19"] (Block [] (Jwa "l16" [])),JumpTarget "l18" ["x17"] (Block [] (Jwa "l20" [Num 1])),JumpTarget "l16" [] (Block [Let "x21" (Sub (Var "x17") (Var "x19"))] (Jwa "l15" [Var "x21"])),JumpTarget "l13" [] (Block [Let "x22" (Cal "addNums" [Var "x14"])] (Jwa "l12" [Var "x22"])),JumpTarget "l10" ["x9"] (Block [] (Jwa "l18" [Var "x"])),JumpTarget "l8" [] (Block [Let "x23" (Add (Var "x9") (Var "x11"))] (Jwa "return" [Var "x23"])),JumpTarget "l0" [] (Block [] (Jwa "return" [Num 0])),JumpTarget "l1" [] (Block [] (Jwa "l10" [Var "x"])),JumpTarget "return" ["result"] (Block [] (End (Var "result")))]])]


