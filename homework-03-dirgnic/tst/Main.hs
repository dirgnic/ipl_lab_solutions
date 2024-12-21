module Main where

import Syntax (
  Exp(..))
import TypeChecking (
  typeCheck, TypeError(..))
import Lowering (
  lower)
import qualified IntermediateRepresentation as IR (
  ANF(Block), Stm(..), App(..), Val(..))
import CodeGeneration (
  codegenModule)

import Test.Tasty (
  defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit (
  testCase, (@?=))
import Test.Tasty.Golden (
  goldenVsFileDiff)

import LLVM.AST (
  Module)

import LLVM.Module (
  withModuleFromAST, writeLLVMAssemblyToFile, File(File))
import LLVM.Context (
  withContext)


main :: IO ()
main = defaultMain (testGroup "tests" [
  typeCheckingTests,
  loweringTests,
  codeGenerationTests])


typeCheckingTests :: TestTree
typeCheckingTests = testGroup "typeChecking" [

  testCase "ten" (
    typeCheck (Num 10) @?= Nothing),

  testCase "addFiveFalse" (
    typeCheck (Add (Num 5) (Boo False)) @?= Just TypeMismatch),

  testCase "unboundVariable" (
    typeCheck (Add (Num 9) (Var "x")) @?= Just NotInScope)]


loweringTests :: TestTree
loweringTests = testGroup "lowering" [

  testCase "ten" (
    lower (Num 10) @?=
           IR.Block [] (IR.Num 10)),

  testCase "shadowing_trivial" (
    lower (Let "x" (Num 5) (Let "x" (Num 100) (Var "x"))) @?=
           IR.Block [] (IR.Num 100)),

  testCase "shadowing_complex" (
    lower (Let "x" (Num 5) (Let "x" (Add (Num 100) (Num 200)) (Var "x"))) @?=
           IR.Block [IR.Let "x0" (IR.Add (IR.Num 100) (IR.Num 200))] (IR.Var "x0"))]


codeGenerationTests :: TestTree
codeGenerationTests = testGroup "codeGeneration" [

  testCodegen "ten" (IR.Block [
    ]
    (IR.Num 10)),

  testCodegen "shadowing" (IR.Block [
    IR.Let "x0" (IR.Add (IR.Num 100) (IR.Num 200))]
    (IR.Var "x0")),

  testCodegen "comparison" (IR.Block [
    IR.Let "x" (IR.Add (IR.Num 5) (IR.Num 10)),
    IR.Let "y" (IR.Sma (IR.Var "x") (IR.Var "x")),
    IR.Let "z" (IR.And (IR.Var "y") (IR.Boo True))]
    (IR.Var "z")),

  testCodegen "mismatch" (IR.Block [
    IR.Let "x" (IR.And (IR.Boo True) (IR.Boo False)),
    IR.Let "y" (IR.Add (IR.Num 123) (IR.Var "x")),
    IR.Let "z" (IR.Add (IR.Var "x") (IR.Var "y"))]
   (IR.Var "z"))]


-- | Test code generation for the given expression. The test will have the given name.
testCodegen :: String -> IR.ANF -> TestTree
testCodegen name expression =
  goldenVsFileDiff name diffCommand (goldenName name) (outputName name) (
    writeModuleAssembly (outputName name) (codegenModule name expression))


goldenName :: String -> FilePath
goldenName name = "test-data/" ++ name ++ ".golden"

outputName :: String -> FilePath
outputName name = "test-data/" ++ name

diffCommand :: String -> String -> [String]
diffCommand ref new = ["diff", "-u", ref, new]


-- | Write out the given module to the given file path.
writeModuleAssembly :: FilePath -> Module -> IO ()
writeModuleAssembly targetFile modul =
  withContext (\context ->
    withModuleFromAST context modul (\modulePtr ->
        writeLLVMAssemblyToFile (File targetFile) modulePtr))

