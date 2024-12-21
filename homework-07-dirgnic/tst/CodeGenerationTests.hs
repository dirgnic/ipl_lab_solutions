module CodeGenerationTests where

import IntermediateRepresentation (
  Program, JoinPoint(..), Block(Block), Stm(..), App(..), Val(..), Jump(..))
import CodeGeneration (
  codegenModule)

import Test.Tasty (
  TestTree, testGroup)
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

  testCodegen "ten" [JoinPoint "entrypoint" (Block [
    ]
    (End (Num 10)))],

  testCodegen "shadowing" [JoinPoint "entrypoint" (Block [
    Let "x0" (Add (Num 100) (Num 200))]
    (End (Var "x0")))],

  testCodegen "comparison" ([JoinPoint "entrypoint" (Block [
    Let "x" (Add (Num 5) (Num 10)),
    Let "y" (Sma (Var "x") (Var "x")),
    Let "z" (And (Var "y") (Boo True))]
    (End (Var "z")))]),

  testCodegen "ifthenelse" ([
    JoinPoint "entrypoint" (Block [
      Let "b" (Sma (Num 5) (Num 7)),
      Let "u" (Alo "r")]
        (Bra (Var "b") "l0" "l1")),
    JoinPoint "l0" (Block [
      Let "x" (Add (Num 4) (Num 13)),
      Let "u" (Sto "r" (Var "x"))]
        (Jum "l2")),
    JoinPoint "l1" (Block [
        Let "u" (Sto "r" (Num 100))]
        (Jum "l2")),
    JoinPoint "l2" (Block [
        Let "a" (Loa "r")]
        (End (Var "a")))]),

  testCodegen "heap" ([JoinPoint "entrypoint" (Block [
    Let "x" (Mal "r"),
    Let "y" (Sto "r" (Num 9)),
    Let "z" (Loa "r")]
    (End (Var "z")))])]


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



