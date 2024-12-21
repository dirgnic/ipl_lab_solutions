{-# LANGUAGE OverloadedStrings #-}
module CodeGeneration where

import IntermediateRepresentation (
  Program, JoinPoint(JoinPoint),
  Block(Block), Stm(Let), App(..), Val(..),
  Jump(..))

import qualified LLVM.AST.Constant as LLVM (
  Constant(Int, GlobalReference))
import qualified LLVM.AST as LLVM (
  Instruction(Add, And, ICmp, Alloca, Load, Store, Call))

import LLVM.AST (
  Module(Module), Definition(GlobalDefinition), BasicBlock(BasicBlock),
  Operand(LocalReference, ConstantOperand),
  functionDefaults, Named(Do, (:=)), mkName,
  Instruction, Terminator(Ret, Br, CondBr), Operand)
import LLVM.AST.Global (
  Global(name, returnType, parameters, basicBlocks), Parameter(Parameter))
import LLVM.AST.CallingConvention (
  CallingConvention(C))
import LLVM.AST.IntegerPredicate (
 IntegerPredicate(ULT))
import LLVM.AST.Type (
  i64, ptr, Type(FunctionType))

import Data.String (
  fromString)


-- | Generate an LLVM module for the given program with name \"myModule\".
codegen :: Program -> Module
codegen program = codegenModule "myModule" program


-- | Generate an LLVM operand from the given trivial expression.
codegenOperand :: Val -> Operand
codegenOperand (Num n) = ConstantOperand (LLVM.Int 64 n)
codegenOperand (Boo False) = ConstantOperand (LLVM.Int 1 0)
codegenOperand (Boo True) = ConstantOperand (LLVM.Int 1 1)
codegenOperand (Var x) = LocalReference i64 (mkName x)


-- | Generate a list of instructions from the given list of bindings.
codegenInstructions :: [Stm] -> [Named Instruction]
codegenInstructions bindings = map codegenInstruction bindings


-- | Generate a single instruction from the given binding.
codegenInstruction :: Stm -> Named Instruction
codegenInstruction (Let x (Add y z)) =
  mkName x := LLVM.Add False False (codegenOperand y) (codegenOperand z) []
codegenInstruction (Let x (And y z)) =
  mkName x := LLVM.And (codegenOperand y) (codegenOperand z) []
codegenInstruction (Let x (Sma y z)) =
  mkName x := LLVM.ICmp ULT (codegenOperand y) (codegenOperand z) []
codegenInstruction (Let _x (Alo r)) =
  mkName r := LLVM.Alloca i64 Nothing 0 []
codegenInstruction (Let x (Loa r)) =
  mkName x := LLVM.Load False i64 (LocalReference ptr (mkName r)) Nothing 0 []
codegenInstruction (Let _x (Sto r e)) =
  Do (LLVM.Store False (LocalReference ptr (mkName r)) (codegenOperand e)  Nothing 0 [])
codegenInstruction (Let _x (Mal r)) = do
  let mallocType = FunctionType ptr [i64] False
  let mallocFunction = Right (ConstantOperand (LLVM.GlobalReference (mkName "malloc")))
  mkName r := LLVM.Call Nothing C [] mallocType mallocFunction [(ConstantOperand (LLVM.Int 64 1),[])] [] []


-- | An LLVM basic block from the given IR expression.
codegenBasicBlock :: JoinPoint -> BasicBlock
codegenBasicBlock (JoinPoint label (Block bindings jump)) = let
  instructions = codegenInstructions bindings
  terminator = codegenTerminator jump
  in BasicBlock (mkName label) instructions terminator


-- | An LLVM terminator that jumps to the next basic block or ends the program.
codegenTerminator :: Jump -> Named Terminator
codegenTerminator (End result) =
  Do (Ret (Just (codegenOperand result)) [])
codegenTerminator (Jum label) =
  Do (Br (mkName label) [])
codegenTerminator (Bra condition label1 label2) =
  Do (CondBr (codegenOperand condition) (mkName label1) (mkName label2) [])


-- | An LLVM basic block from the given IR expression.
codegenBasicBlocks :: Program -> [BasicBlock]
codegenBasicBlocks program =
  map codegenBasicBlock program


-- | An LLVM definition of a single function \"myMain\" for the given program.
codegenDefinition :: Program -> Definition
codegenDefinition program = GlobalDefinition (functionDefaults {
  name = "myMain",
  returnType = i64,
  basicBlocks = codegenBasicBlocks program})


-- | An LLVM module with the given name for the given program.
-- It consists of a single function definition.
codegenModule :: String -> Program -> Module
codegenModule moduleName program =
  Module (fromString moduleName) (fromString (moduleName ++ ".program")) Nothing Nothing [mallocDeclaration, codegenDefinition program]

-- | Declaration of extern function malloc
mallocDeclaration :: Definition
mallocDeclaration = GlobalDefinition (functionDefaults {
  returnType = ptr,
  parameters = ([Parameter i64 (mkName "size") []], False),
  name = mkName "malloc"})


