module CodeGenerationTests where

import Syntax (
  Type(TypeInt))
import IntermediateRepresentation (
  Program, Function(Function),
  JumpTarget(..), Block(Block), Binding(..), App(..), Val(..), Jump(..))
import CodeGeneration (
  codegenModule, transposePredecessors)

import Test.Tasty (
  TestTree, testGroup)
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


codeGenerationTests :: TestTree
codeGenerationTests = testGroup "codeGeneration" [

  testGroup "transposePredecessors" [
    testCase "basicExample" (
      transposePredecessors [("l1", [Num 1, Num 3]), ("l2", [Var "x", Var "y"])] @?=
      [[("l1",Num 1), ("l2",Var "x")], [("l1",Num 3), ("l2",Var "y")]])],

  testCodegen "ten" [Function "myMain" [] TypeInt [
            JumpTarget "entrypoint" [] (Block [
              ] (Jwa "return" [Num 10])),
            JumpTarget "return" ["result"] (Block [
              ] (End (Var "result")))]],

  testCodegen "comparison" [Function "myMain" [] TypeInt [JumpTarget "entrypoint" [] (Block [] (Jwa "l2" [Num 15])),JumpTarget "l4" ["x3"] (Block [] (Jwa "l0" [])),JumpTarget "l2" ["x1"] (Block [] (Jwa "l4" [Num 8])),JumpTarget "l0" [] (Block [Let "x5" (Sma (Var "x1") (Var "x3"))] (Jwa "return" [Var "x5"])),JumpTarget "return" ["result"] (Block [] (End (Var "result")))]],

  testCodegen "ifthenelse" [Function "myMain" [] TypeInt [JumpTarget "entrypoint" [] (Block [] (Jwa "l4" [Num 7])),JumpTarget "l6" ["x5"] (Block [] (Jwa "l2" [])),JumpTarget "l4" ["x3"] (Block [] (Jwa "l6" [Num 8])),JumpTarget "l2" [] (Block [Let "x7" (Sma (Var "x3") (Var "x5"))] (Bwa (Var "x7") "l0" [] "l1" [])),JumpTarget "l0" [] (Block [] (Jwa "return" [Num 1])),JumpTarget "l1" [] (Block [] (Jwa "return" [Num 2])),JumpTarget "return" ["result"] (Block [] (End (Var "result")))]],

  testCodegen "function" [Function "myMain" [] TypeInt [JumpTarget "entrypoint" [] (Block [] (Jwa "l2" [Num 10])),JumpTarget "l2" ["x1"] (Block [] (Jwa "l0" [])),JumpTarget "l0" [] (Block [Let "x3" (Cal "f" [Var "x1"])] (Jwa "return" [Var "x3"])),JumpTarget "return" ["result"] (Block [] (End (Var "result")))],Function "f" [("x", TypeInt)] TypeInt [JumpTarget "entrypoint" [] (Block [] (Jwa "return" [Var "x"])),JumpTarget "return" ["result"] (Block [] (End (Var "result")))]],

  testCodegen "addNums" [Function "myMain" [] TypeInt [JumpTarget "entrypoint" [] (Block [] (Jwa "l2" [Num 10])),JumpTarget "l2" ["x1"] (Block [] (Jwa "l0" [])),JumpTarget "l0" [] (Block [Let "x3" (Cal "addNums" [Var "x1"])] (Jwa "return" [Var "x3"])),JumpTarget "return" ["result"] (Block [] (End (Var "result")))],Function "addNums" [("x", TypeInt)] TypeInt [JumpTarget "entrypoint" [] (Block [] (Jwa "l4" [Var "x"])),JumpTarget "l6" ["x5"] (Block [] (Jwa "l2" [])),JumpTarget "l4" ["x3"] (Block [] (Jwa "l6" [Num 1])),JumpTarget "l2" [] (Block [Let "x7" (Sma (Var "x3") (Var "x5"))] (Bwa (Var "x7") "l0" [] "l1" [])),JumpTarget "l12" ["x11"] (Block [] (Jwa "l8" [])),JumpTarget "l15" ["x14"] (Block [] (Jwa "l13" [])),JumpTarget "l20" ["x19"] (Block [] (Jwa "l16" [])),JumpTarget "l18" ["x17"] (Block [] (Jwa "l20" [Num 1])),JumpTarget "l16" [] (Block [Let "x21" (Sub (Var "x17") (Var "x19"))] (Jwa "l15" [Var "x21"])),JumpTarget "l13" [] (Block [Let "x22" (Cal "addNums" [Var "x14"])] (Jwa "l12" [Var "x22"])),JumpTarget "l10" ["x9"] (Block [] (Jwa "l18" [Var "x"])),JumpTarget "l8" [] (Block [Let "x23" (Add (Var "x9") (Var "x11"))] (Jwa "return" [Var "x23"])),JumpTarget "l0" [] (Block [] (Jwa "return" [Num 0])),JumpTarget "l1" [] (Block [] (Jwa "l10" [Var "x"])),JumpTarget "return" ["result"] (Block [] (End (Var "result")))]]]


-- | Test code generation for the given program. The test will have the given name.
testCodegen :: String -> Program -> TestTree
testCodegen name program =
  goldenVsFileDiff name diffCommand (goldenName name) (outputName name) (
    writeModuleAssembly (outputName name) (codegenModule name program))


goldenName :: String -> FilePath
goldenName name = "test-data/codegen/" ++ name ++ ".golden"

outputName :: String -> FilePath
outputName name = "test-data/codegen/" ++ name

diffCommand :: String -> String -> [String]
diffCommand ref new = ["diff", "-u", ref, new]


-- | Write out the given module to the given file path.
writeModuleAssembly :: FilePath -> Module -> IO ()
writeModuleAssembly targetFile modul =
  withContext (\context ->
    withModuleFromAST context modul (\modulePtr ->
        writeLLVMAssemblyToFile (File targetFile) modulePtr))



