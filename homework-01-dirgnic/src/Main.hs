{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified LLVM.AST.Constant as LLVM (Constant(..))
import LLVM.AST (
  Module(Module), Definition(GlobalDefinition), BasicBlock(BasicBlock),
  functionDefaults,
  Named(Do), Terminator(Ret), Operand(ConstantOperand))
import LLVM.AST.Global (
  Global(name, returnType, basicBlocks))
import LLVM.AST.Type (i1)
import LLVM.Module (
  withModuleFromAST, writeObjectToFile, File(File))
import LLVM.Context (withContext)
import LLVM.Target (withHostTargetMachineDefault)

-- | A language of boolean expressions with xor.
data Exp =
  Lit Bool |
  And Exp Exp |
  Or Exp Exp |
  Not Exp |
  Xor Exp Exp

-- | Generate an LLVM constant from a boolean expression.
codegen :: Exp -> LLVM.Constant
codegen (Lit False) = LLVM.Int 1 0  -- i1 false (0)
codegen (Lit True) = LLVM.Int 1 1   -- i1 true (1)
codegen (And lhs rhs) = LLVM.And (codegen' lhs) (codegen' rhs)
codegen (Or lhs rhs) = LLVM.Or (codegen' lhs) (codegen' rhs)
codegen (Xor lhs rhs) = LLVM.Xor (codegen' lhs) (codegen' rhs)
codegen (Not e) = LLVM.Xor (codegen' e) (LLVM.Int 1 1)  -- NOT as XOR with 1

-- | Helper function to wrap LLVM.Constant as Operand.
codegen' :: Exp -> LLVM.Constant
codegen' = codegen

-- | Example expression.
myExpression :: Exp
myExpression = Xor (Lit True) (And (Lit False) (Lit True))

-- | Generate LLVM constant from 'myExpression'.
myConstant :: LLVM.Constant
myConstant = codegen myExpression

-- | An LLVM terminator that returns a constant operand.
myTerminator :: Terminator
myTerminator = Ret (Just (ConstantOperand myConstant)) []

-- | An LLVM basic block.
myBasicBlock :: BasicBlock
myBasicBlock = BasicBlock "myEntrypoint" [] (Do myTerminator)

-- | Define a single function in LLVM.
myDefinition :: Definition
myDefinition = GlobalDefinition (functionDefaults {
  name = "myMain",
  returnType = i1,
  basicBlocks = [myBasicBlock]})

-- | Combine into an LLVM module.
myModule :: Module
myModule = Module "myModule" "myModule.be" Nothing Nothing [myDefinition]

-- | Write an object file for the module.
main :: IO ()
main = do
  withContext (\context ->
    withModuleFromAST context myModule (\modulePtr ->
      withHostTargetMachineDefault (\targetMachine ->
        writeObjectToFile targetMachine (File "myModule.o") modulePtr)))
