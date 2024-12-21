{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hw02 (Exp(..), codegenModule)
import LLVM.Module (withModuleFromAST, writeObjectToFile, File(File))
import LLVM.Context (withContext)
import LLVM.Target (withHostTargetMachineDefault)
import LLVM.AST (Module)

-- | A helper function to compile an LLVM module to an object file.
compileModule :: FilePath -> Module -> IO ()
compileModule targetFile modul = do
  withContext $ \context ->
    withModuleFromAST context modul $ \modulePtr ->
      withHostTargetMachineDefault $ \targetMachine ->
        writeObjectToFile targetMachine (File targetFile) modulePtr

-- | Test case: Multiplication of two expressions.
-- (2 + 3) * 4
myExpressionMul :: Exp
myExpressionMul = Mul (Add (Lit 2) (Lit 3)) (Lit 4)

-- Generate the LLVM module for the multiplication test case.
myModuleMul :: Module
myModuleMul = codegenModule "testMul" myExpressionMul

-- | Test case: Shadowing of variables.
-- let x = 5 in let x = x + 3 in x * 2
myExpressionShadowing :: Exp
myExpressionShadowing = Let "x" (Lit 5) (Let "x" (Add (Var "x") (Lit 3)) (Mul (Var "x") (Lit 2)))

-- Generate the LLVM module for the shadowing test case.
myModuleShadowing :: Module
myModuleShadowing = codegenModule "testShadowing" myExpressionShadowing

-- | The main function to compile and generate object files for tests.
main :: IO ()
main = do
  putStrLn "Generating testMul.o for the multiplication test case..."
  compileModule "testMul.o" myModuleMul
  putStrLn "Generated testMul.o successfully."

  putStrLn "Generating testShadowing.o for the shadowing test case..."
  compileModule "testShadowing.o" myModuleShadowing
  putStrLn "Generated testShadowing.o successfully."
