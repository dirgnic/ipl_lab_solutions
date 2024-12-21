{-# LANGUAGE OverloadedStrings #-}

module Main where


import qualified Syntax as S -- Qualified import for Syntax module
import TypeChecking (infer, TypeError(..), Exp(..), Type(..)) -- Import for type checking logic
import Lowering (lower) -- Lowering to intermediate representation
import qualified IntermediateRepresentation as IR (ANF(Block), Stm(..), App(..), Val(..)) -- IR definitions
import CodeGeneration (codegenModule) -- LLVM code generation
import qualified Data.Map as Map -- Type environment handling

import qualified Data.ByteString.Char8 as BS
import LLVM.Module (moduleLLVMAssembly)

-- LLVM imports
import LLVM.AST (Module) -- LLVM module type
import qualified LLVM.AST as LLVM -- Qualified import for LLVM-specific names
import LLVM.Module (withModuleFromAST, writeLLVMAssemblyToFile, File(File)) -- LLVM module handling
import LLVM.Context (withContext) -- LLVM context management
import LLVM.Target (initializeAllTargets, withHostTargetMachineDefault) -- Target setup

-- Test framework imports
import Test.Tasty (TestTree, testGroup, defaultMain) -- Test grouping
import Test.Tasty.HUnit (testCase, (@?=)) -- Unit testing
import Test.Tasty.Golden (goldenVsFileDiff) -- Golden tests

main :: IO ()
main = do
  -- Initialize LLVM targets
  initializeAllTargets

  -- Example expression
  let exampleExpression = S.Let "x" (S.Add (S.IntLit 10) (S.IntLit 20)) (S.Var "x")

  -- Lower the expression to ANF
  let loweredExpression = lower exampleExpression
  let llvmModule = codegenModule "myModule" loweredExpression

  -- Write the LLVM IR for debugging
  putStrLn "Generating debug.ll..."
  writeModuleAssembly "debug.ll" llvmModule
  putStrLn "debug.ll generated successfully!"

  -- Run tests
  putStrLn "Running tests..."
  defaultMain tests

-- Group all tests
tests :: TestTree
tests = testGroup "tests" [typeCheckingTests, loweringTests, codeGenerationTests]




writeModuleAssembly :: FilePath -> LLVM.Module -> IO ()
writeModuleAssembly targetFile llvmModule = do
  withContext $ \context ->
    withModuleFromAST context llvmModule $ \modulePtr -> do
      -- Write LLVM assembly to the file
      writeLLVMAssemblyToFile (File targetFile) modulePtr
      -- Print LLVM IR to the console
      llvmIR <- moduleLLVMAssembly modulePtr
      BS.putStrLn llvmIR

-- Type checking tests
typeCheckingTests :: TestTree
typeCheckingTests = testGroup "typeChecking" [
  testCase "ten" (
    infer Map.empty (IntLit 10) @?= Right TInt),
  testCase "addFiveFalse" (
    infer Map.empty (TypeChecking.Add (IntLit 5) (BoolLit False)) @?= Left "Expected both operands to be TInt"),
  testCase "subValid" (
    infer Map.empty (TypeChecking.Sub (IntLit 10) (IntLit 5)) @?= Right TInt),
  testCase "notValid" (
    infer Map.empty (Not (BoolLit True)) @?= Right TBool),
  testCase "eqValid" (
    infer Map.empty (Eq (IntLit 5) (IntLit 5)) @?= Right TBool),
  testCase "unboundVariable" (
    infer Map.empty (TypeChecking.Add (IntLit 9) (Var "x")) @?= Left "Variable not found: x")]

-- Lowering tests
loweringTests :: TestTree
loweringTests = testGroup "lowering" [
  testCase "ten" (
    lower (S.IntLit 10) @?= IR.Block [] (IR.Num 10)),
  testCase "sub" (
    lower (S.Sub (S.IntLit 10) (S.IntLit 5)) @?=
    IR.Block [IR.Let "x0" (IR.Sub (IR.Num 10) (IR.Num 5))] (IR.Var "x0")),
  testCase "not" (
    lower (S.Not (S.BoolLit True)) @?=
    IR.Block [IR.Let "x0" (IR.Not (IR.Boo True))] (IR.Var "x0")),
  testCase "eq" (
    lower (S.Eq (S.IntLit 5) (S.IntLit 5)) @?=
    IR.Block [IR.Let "x0" (IR.Eq (IR.Num 5) (IR.Num 5))] (IR.Var "x0")),
  testCase "shadowing_trivial" (
    lower (S.Let "x" (S.IntLit 5) (S.Let "x" (S.IntLit 100) (S.Var "x"))) @?=
    IR.Block [] (IR.Num 100)),
  testCase "shadowing_complex" (
    lower (S.Let "x" (S.IntLit 5) (S.Let "x" (S.Add (S.IntLit 100) (S.IntLit 200)) (S.Var "x"))) @?=
    IR.Block [IR.Let "x0" (IR.Add (IR.Num 100) (IR.Num 200))] (IR.Var "x0"))]

-- Code generation tests
codeGenerationTests :: TestTree
codeGenerationTests = testGroup "codeGeneration" [
  testCodegen "ten" (IR.Block [] (IR.Num 10)),
  testCodegen "sub" (IR.Block [
    IR.Let "x0" (IR.Sub (IR.Num 10) (IR.Num 5))]
    (IR.Var "x0")),
  testCodegen "not" (IR.Block [
    IR.Let "x0" (IR.Not (IR.Boo True))]
    (IR.Var "x0")),
  testCodegen "eq" (IR.Block [
    IR.Let "x0" (IR.Eq (IR.Num 5) (IR.Num 5))]
    (IR.Var "x0")),
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

-- Helper to test code generation
testCodegen :: String -> IR.ANF -> TestTree
testCodegen name expression =
  goldenVsFileDiff name diffCommand (goldenName name) (outputName name) (
    writeModuleAssembly (outputName name ++ ".ll") (codegenModule name expression))

goldenName :: String -> FilePath
goldenName name = "test-data/" ++ name ++ ".golden"

outputName :: String -> FilePath
outputName name = "test-data/" ++ name

diffCommand :: String -> String -> [String]
diffCommand ref new = ["diff", "-u", ref, new]
