{-# LANGUAGE OverloadedStrings #-}
module Main where

import LLVM.AST (
  Module(Module), Definition(GlobalDefinition), BasicBlock(BasicBlock),
  functionDefaults,
  Named(Do), Terminator(Ret), Operand(ConstantOperand))
import LLVM.AST.Constant (
  Constant(Int))
import LLVM.AST.Global (
  Global(name, returnType, basicBlocks))
import LLVM.AST.Type (
  i64)

import LLVM.Module (
  withModuleFromAST, writeObjectToFile, File(File))
import LLVM.Context (
  withContext)
import LLVM.Target (
  withHostTargetMachineDefault)


myModule :: Module
myModule = Module "myModule" "myModule.ae" Nothing Nothing [myDefinition]

myDefinition :: Definition
myDefinition = GlobalDefinition (functionDefaults {
  name = "myMain",
  returnType = i64,
  basicBlocks = [myBasicBlock]})

myBasicBlock :: BasicBlock
myBasicBlock = BasicBlock "myEntrypoint" [] (Do myTerminator)

myTerminator :: Terminator
myTerminator = Ret (Just myOperand) []

myOperand :: Operand
myOperand = ConstantOperand myConstant

myConstant :: Constant
myConstant = Int 64 5

main :: IO ()
main = do
  withContext (\context ->
    withModuleFromAST context myModule (\modulePtr ->
      withHostTargetMachineDefault (\targetMachine ->
        writeObjectToFile targetMachine (File "myModule.o") modulePtr)))

