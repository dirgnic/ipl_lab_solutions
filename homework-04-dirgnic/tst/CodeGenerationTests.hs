module CodeGenerationTests where

import IntermediateRepresentation (
  Program, JumpTarget(..), Block(Block), Binding(..), App(..), Val(..), Jump(..))
import CodeGeneration (
  codegenModule, transposePredecessors, findPredecessors)

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

  testGroup "findPredecessors" [
    testCase "joinPoint" (
      findPredecessors [
        JumpTarget "l0" [] (Block [] (Jwa "l2" [Num 4, Var "z"])),
        JumpTarget "l1" [] (Block [] (Jwa "l2" [Num 9, Num 2])),
        JumpTarget "l2" ["x", "y"] (Block [] (End (Num 8)))] (
          JumpTarget "l2" ["x", "y"] (Block [] (End (Num 8)))) @?=
      [("l0", [Num 4, Var "z"]),("l1", [Num 9, Num 2])]),
    testCase "branch" (
      findPredecessors [
        JumpTarget "l0" [] (Block [] (Bwa (Boo False) "l1" [Var "x", Var "y"] "l2" [Num 3])),
        JumpTarget "l1" ["a", "b"] (Block [] (End (Num 9))),
        JumpTarget "l2" ["x"] (Block [] (End (Num 8)))] (
          JumpTarget "l2" ["x"] (Block [] (End (Num 8)))) @?=
      [("l0", [Num 3])]),
    testCase "loop" (
      findPredecessors [
        JumpTarget "l0" [] (Block [] (Jwa "l0" []))] (
          JumpTarget "l0" [] (Block [] (Jwa "l0" []))) @?=
      [("l0",[])])
  ],

  testCodegen "ten" [JumpTarget "entrypoint" [] (Block [
    ]
    (End (Num 10)))],

  testCodegen "shadowing" [JumpTarget "entrypoint" [] (Block [
    Let "x0" (Add (Num 100) (Num 200))]
    (End (Var "x0")))],

  testCodegen "comparison" ([JumpTarget "entrypoint" [] (Block [
    Let "x" (Add (Num 5) (Num 10)),
    Let "y" (Sma (Var "x") (Var "x")),
    Let "z" (And (Var "y") (Boo True))]
    (End (Var "z")))]),

  testCodegen "ifthenelse" ([
    JumpTarget "entrypoint" [] (Block [
      Let "b" (Sma (Num 5) (Num 7))]
        (Bwa (Var "b") "l0" [] "l1" [])),
    JumpTarget "l0" [] (Block [
      Let "x" (Add (Num 4) (Num 13))]
        (Jwa "l2" [Var "x"])),
    JumpTarget "l1" [] (Block []
        (Jwa "l2" [Num 100])),
    JumpTarget "l2" ["a"] (Block []
        (End (Var "a")))])]


-- | Test code generation for the given program. The test will have the given name.
testCodegen :: String -> Program -> TestTree
testCodegen name program =
  goldenVsFileDiff name diffCommand (goldenName name) (outputName name) (
    writeModuleAssembly (outputName name) (codegenModule name program))


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



