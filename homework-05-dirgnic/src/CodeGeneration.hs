{-# LANGUAGE OverloadedStrings #-}
module CodeGeneration where

import IntermediateRepresentation (Program, JumpTarget(..), Block(..), Binding(..), App(..), Val(..), Jump(..))

import qualified LLVM.AST.Constant as LLVM (
  Constant(Int))
import qualified LLVM.AST as LLVM (
  Instruction(Add, And, ICmp, Phi))

import LLVM.AST (
  Module(Module), Definition(GlobalDefinition), BasicBlock(BasicBlock),
  Operand(LocalReference, ConstantOperand),
  functionDefaults, Named(Do, (:=)), mkName,
  Instruction, Terminator(Ret, Br, CondBr), Operand)
import LLVM.AST.Global (
  Global(name, returnType, basicBlocks))
import LLVM.AST.IntegerPredicate (
 IntegerPredicate(ULT))
import LLVM.AST.Type (
  i64)

import Data.List (
  transpose)
import Control.Monad (
  guard)
import Data.String (
  fromString)

import qualified IntermediateRepresentation as IR



-- | Generate an LLVM module for the given program with name \"myModule\".
codegen :: Program -> Module
codegen program = codegenModule "myModule" program


-- | An LLVM module with the given name for the given program.
-- It consists of a single function definition.
codegenModule :: String -> Program -> Module
codegenModule moduleName program =
  Module (fromString moduleName) (fromString (moduleName ++ ".program")) Nothing Nothing [codegenDefinition program]


-- | An LLVM definition of a single function \"myMain\" for the given program.
codegenDefinition :: Program -> Definition
codegenDefinition program = GlobalDefinition (functionDefaults {
  name = "myMain",
  returnType = i64,
  basicBlocks = codegenBasicBlocks program})


-- | An LLVM basic block from the given jump target.
codegenBasicBlocks :: Program -> [BasicBlock]
codegenBasicBlocks program =
  map (\jumpTarget -> codegenBasicBlock (findPredecessors program jumpTarget) jumpTarget) program




type Predecessors = [(String, [Val])] 
-- | An LLVM basic block from the given jump target.
codegenBasicBlock :: Predecessors -> JumpTarget -> BasicBlock
codegenBasicBlock predecessors (JumpTarget label parameters (Block bindings jump)) = do
  let phiInstructions = codegenPhiInstructions parameters predecessors
  let blockInstructions = codegenInstructions bindings
  let instructions = phiInstructions ++ blockInstructions
  let terminator = codegenTerminator jump
  BasicBlock (mkName label) instructions terminator


 -- Replace Label with Stringd
codegenPhiInstructions :: [String] -> Predecessors -> [Named Instruction]
codegenPhiInstructions variables predecessors =
  zipWith codegenPhiInstruction variables (transposePredecessors predecessors)

codegenPhiInstruction :: String -> [(String, Val)] -> Named Instruction
codegenPhiInstruction variable predecessors = do
  let incoming = map (\(label, value) -> (codegenOperand value, mkName label)) predecessors
  mkName variable := LLVM.Phi i64 incoming []

-- | Given an association list of predecessor label to list of arguments, returns
-- a list of lists of pairs of label and argument.
-- All predecessors have to be associated to the same number of arguments.
-- The outer list has as many elements as there are arguments.
-- The inner lists have as many elements as there are predecessors.
transposePredecessors :: Predecessors -> [[(String, Val)]]
transposePredecessors predecessors =
  transpose (map (\(label, arguments) -> map (\argument -> (label, argument)) arguments) predecessors)


-- | An LLVM terminator that jumps to the next basic block or ends the program.
codegenTerminator :: Jump -> Named Terminator
codegenTerminator (End result) =
  Do (Ret (Just (codegenOperand result)) [])
codegenTerminator (Jwa label _) =
  Do (Br (mkName label) [])
codegenTerminator (Bwa condition label1 _ label2 _) =
  Do (CondBr (codegenOperand condition) (mkName label1) (mkName label2) [])


-- | Generate an LLVM operand from the given trivial expression.
codegenOperand :: Val -> Operand
codegenOperand (IR.Num n)       = ConstantOperand (LLVM.Int 64 n)
codegenOperand (IR.Boo False)   = ConstantOperand (LLVM.Int 1 0)
codegenOperand (IR.Boo True)    = ConstantOperand (LLVM.Int 1 1)
codegenOperand (IR.Var x)       = LocalReference i64 (mkName x)

-- | Generate a list of instructions from the given list of bindings.
codegenInstructions :: [Binding] -> [Named Instruction]
codegenInstructions bindings = map codegenInstruction bindings


-- | Generate a single instruction from the given binding.
codegenInstruction :: Binding -> Named Instruction
codegenInstruction (Let x (Add y z)) =
  mkName x := LLVM.Add False False (codegenOperand y) (codegenOperand z) []
codegenInstruction (Let x (And y z)) =
  mkName x := LLVM.And (codegenOperand y) (codegenOperand z) []
codegenInstruction (Let x (Sma y z)) =
  mkName x := LLVM.ICmp ULT (codegenOperand y) (codegenOperand z) []


-- | Find for the given jump target the list of predecessors.
findPredecessors :: Program -> JumpTarget -> Predecessors
findPredecessors program (JumpTarget targetLabel _ _) = do
  JumpTarget predecessorLabel _ (Block _ jump) <- program
  case jump of
    End _ ->
      []
    Jwa jumpLabel arguments -> do
      guard (jumpLabel == targetLabel)
      [(predecessorLabel, arguments)]
    Bwa _ jumpLabel1 arguments1 jumpLabel2 arguments2 -> do
      let predecessor1 = do
            guard (jumpLabel1 == targetLabel)
            return (predecessorLabel, arguments1)
      let predecessor2 = do
            guard (jumpLabel2 == targetLabel)
            return (predecessorLabel, arguments2)
      predecessor1 ++ predecessor2

