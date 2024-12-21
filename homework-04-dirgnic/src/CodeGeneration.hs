{-# LANGUAGE OverloadedStrings #-}
module CodeGeneration where

import IntermediateRepresentation (
  Program, JumpTarget(JumpTarget), Var, Label,
  Block(Block), Binding(Let), App(..), Val(..),
  Jump(..))

import qualified LLVM.AST.Constant as LLVM (
  Constant(Int))
import qualified LLVM.AST as LLVM (
  Instruction(Add, And, ICmp, Phi, Sub), Named(Do, (:=)), Operand(LocalReference, ConstantOperand),
  Terminator(Ret, Br, CondBr), Module(Module), BasicBlock(BasicBlock), Definition(..))
import LLVM.AST.IntegerPredicate (IntegerPredicate(ULT))
import LLVM.AST.Type (i64)
import LLVM.AST.Global (Global(name, returnType, basicBlocks))

import qualified LLVM.AST.Global as G

import Data.List (transpose)
import Control.Monad (guard)
import Data.String (fromString)

-- | Generate an LLVM module for the given program with name \"myModule\".
codegen :: Program -> LLVM.Module
codegen program = codegenModule "myModule" program

-- | An LLVM module with the given name for the given program.
-- It consists of a single function definition.

codegenModule :: String -> Program -> LLVM.Module
codegenModule moduleName program =
  LLVM.Module
    (fromString moduleName)
    (fromString (moduleName ++ ".program"))
    Nothing
    Nothing
    [codegenDefinition program] -- `Definition` wraps the function


-- | An LLVM definition of a single function \"myMain\" for the given program.


codegenDefinition :: Program -> LLVM.Definition
codegenDefinition program =
  LLVM.GlobalDefinition G.functionDefaults {
    G.name        = fromString "myMain",
    G.returnType  = i64,
    G.basicBlocks = codegenBasicBlocks program
  }


-- | An LLVM basic block from the given jump target.
codegenBasicBlocks :: Program -> [LLVM.BasicBlock]
codegenBasicBlocks program =
  map (\jumpTarget -> codegenBasicBlock (findPredecessors program jumpTarget) jumpTarget) program

type Predecessors = [(Label, [Val])]

-- | An LLVM basic block from the given jump target.
codegenBasicBlock :: Predecessors -> JumpTarget -> LLVM.BasicBlock
codegenBasicBlock predecessors (JumpTarget label parameters (Block bindings jump)) = do
  let phiInstructions = codegenPhiInstructions parameters predecessors
  let blockInstructions = codegenInstructions bindings
  let instructions = phiInstructions ++ blockInstructions
  let terminator = codegenTerminator jump
  LLVM.BasicBlock (fromString label) instructions terminator

-- | Generate the phi instructions at the beginning of a basic block.
codegenPhiInstructions :: [Var] -> Predecessors -> [LLVM.Named LLVM.Instruction]
codegenPhiInstructions variables predecessors =
  zipWith codegenPhiInstruction variables (transposePredecessors predecessors)

-- | Generate a single phi instructions that binds the given variable.

codegenPhiInstruction :: Var -> [(Label, Val)] -> LLVM.Named LLVM.Instruction
codegenPhiInstruction variable predecessors =
  let incoming = map (\(label, value) -> (codegenOperand value, fromString label)) predecessors
      -- Add a default value if there are no predecessors
      defaultIncoming = (LLVM.ConstantOperand (LLVM.Int 64 0), fromString "default")
  in fromString variable LLVM.:= LLVM.Phi i64 (if null incoming then [defaultIncoming] else incoming) []

-- | Transpose predecessor arguments for PHI instructions.
transposePredecessors :: Predecessors -> [[(Label, Val)]]
transposePredecessors predecessors =
  transpose (map (\(label, arguments) -> map (\argument -> (label, argument)) arguments) predecessors)

-- | Generate an LLVM terminator that jumps to the next basic block or ends the program.

codegenTerminator :: Jump -> LLVM.Named LLVM.Terminator
codegenTerminator (End result) =
  LLVM.Do (LLVM.Ret (Just (codegenOperand result)) [])
codegenTerminator (Jwa label _) =
  LLVM.Do (LLVM.Br (fromString label) [])
codegenTerminator (Bwa condition label1 _ label2 _) =
  LLVM.Do (LLVM.CondBr (codegenOperand condition) (fromString label1) (fromString label2) [])

-- | Generate an LLVM operand from the given trivial expression.
codegenOperand :: Val -> LLVM.Operand
codegenOperand (Num n) = LLVM.ConstantOperand (LLVM.Int 64 n)
codegenOperand (Boo False) = LLVM.ConstantOperand (LLVM.Int 1 0)
codegenOperand (Boo True) = LLVM.ConstantOperand (LLVM.Int 1 1)
codegenOperand (Var x) = LLVM.LocalReference i64 (fromString x)

-- | Generate instructions for the `For` and optional `Brk` constructs.
codegenInstructions :: [Binding] -> [LLVM.Named LLVM.Instruction]
codegenInstructions bindings = concatMap codegenInstruction bindings

-- | Generate a single instruction from the given binding.
codegenInstruction :: Binding -> [LLVM.Named LLVM.Instruction]
codegenInstruction (Let x (Add y z)) =
  [fromString x LLVM.:= LLVM.Add False False (codegenOperand y) (codegenOperand z) []]
codegenInstruction (Let x (And y z)) =
  [fromString x LLVM.:= LLVM.And (codegenOperand y) (codegenOperand z) []]
codegenInstruction (Let x (Sma y z)) =
  [fromString x LLVM.:= LLVM.ICmp ULT (codegenOperand y) (codegenOperand z) []]
codegenInstruction (Let _ (Brk value)) =
  error "Brk should not appear here; it is handled in the terminator."
codegenInstruction (Let _ (For iter var initVal _)) =
  generateForInstructions iter var initVal


-- | Generate a `For` loop.

generateForInstructions :: Val -> Var -> Val -> [LLVM.Named LLVM.Instruction]
generateForInstructions iter var initVal =
  let loopVar = fromString var
      iterOperand = codegenOperand iter
      initOperand = codegenOperand initVal
  in [loopVar LLVM.:= LLVM.Add False False initOperand iterOperand []]

generateForLoop :: Val -> Var -> Val -> [LLVM.Named LLVM.Instruction] -> [LLVM.BasicBlock]
generateForLoop iter var initVal bodyInstructions =
  let loopVar = fromString var
      iterOperand = codegenOperand iter
      initOperand = codegenOperand initVal
      -- Define the loop entry block
      loopHeader = LLVM.BasicBlock
        (fromString "loop_header")
        [loopVar LLVM.:= LLVM.Add False False initOperand iterOperand []]
        (LLVM.Do (LLVM.Br (fromString "loop_body") []))

      -- Define the loop body block
      loopBody = LLVM.BasicBlock
        (fromString "loop_body")
        bodyInstructions
        (LLVM.Do (LLVM.Br (fromString "loop_header") []))
  in [loopHeader, loopBody]

-- | Find for the given jump target the list of predecessors.

findPredecessors :: Program -> JumpTarget -> Predecessors
findPredecessors program (JumpTarget targetLabel _ _) =
  let
    findJumps (JumpTarget label params (Block _ jump)) =
      case jump of
        Jwa nextLabel args
          | nextLabel == targetLabel -> [(label, args)]
        Bwa _ trueLabel trueArgs falseLabel falseArgs
          | trueLabel == targetLabel -> [(label, trueArgs)]
          | falseLabel == targetLabel -> [(label, falseArgs)]
        _ -> []
  in concatMap findJumps program
