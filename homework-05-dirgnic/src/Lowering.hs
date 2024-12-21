module Lowering (lower) where

import Syntax (Stm(..), Exp(..)) -- Import Syntax types unqualified
import qualified IntermediateRepresentation as IR
  ( Program, JumpTarget(..), Block(..), Binding(..), App(..), Val(..), Jump(..), Var )

import Control.Monad.Trans.State (State, get, put, evalState)

-- | Lower a statement into the intermediate representation.
lower :: Stm -> IR.Program
lower statement =
  let returnJumpTarget = IR.JumpTarget "return" ["result"] (IR.Block [] (IR.End (IR.Var "result")))
      (entryBlock, program) = evalState (lowerStm statement "return") 0
   in IR.JumpTarget "entrypoint" [] entryBlock : returnJumpTarget : program

-- | Lower a statement to a block and program using state.
lowerStm :: Stm -> IR.Var -> State Int (IR.Block, IR.Program)
lowerStm (Res result) ret = do
  (bindings, trivial) <- lowerExp result
  return (IR.Block bindings (IR.Jwa ret [trivial]), [])

lowerStm (Let var rhs body) ret = do
  -- Convert rhs using lowerExp
  (rhsBindings, rhsResult) <- lowerExp rhs
  bodyLabel <- freshLabel
  (bodyBlock, bodyProgram) <- lowerStm body ret
  let rhsBlock = IR.Block rhsBindings (IR.Jwa bodyLabel [rhsResult])
  let bodyJumpTarget = IR.JumpTarget bodyLabel [var] bodyBlock
  return (rhsBlock, bodyJumpTarget : bodyProgram)

lowerStm (Ite condition thenStm elseStm) ret = do
  (conditionBindings, conditionResult) <- lowerExp condition
  thenLabel <- freshLabel
  elseLabel <- freshLabel
  (thenBlock, thenProgram) <- lowerStm thenStm ret
  (elseBlock, elseProgram) <- lowerStm elseStm ret
  let conditionBlock = IR.Block conditionBindings (IR.Bwa conditionResult thenLabel [] elseLabel [])
  let thenJumpTarget = IR.JumpTarget thenLabel [] thenBlock
  let elseJumpTarget = IR.JumpTarget elseLabel [] elseBlock
  return (conditionBlock, thenJumpTarget : elseJumpTarget : thenProgram ++ elseProgram)

-- | Lower an expression into intermediate representation.
lowerExp :: Exp -> State Int ([IR.Binding], IR.Val)
lowerExp (Var x) = return ([], IR.Var x)
lowerExp (Num n) = return ([], IR.Num n)
lowerExp (Boo b) = return ([], IR.Boo b)
lowerExp (Add lhs rhs) = lowerBinary IR.Add lhs rhs
lowerExp (And lhs rhs) = lowerBinary IR.And lhs rhs
lowerExp (Sma lhs rhs) = lowerBinary IR.Sma lhs rhs

-- | Helper function to lower binary operations.
lowerBinary :: (IR.Val -> IR.Val -> IR.App) -> Exp -> Exp -> State Int ([IR.Binding], IR.Val)
lowerBinary op lhs rhs = do
  (lhsBindings, lhsResult) <- lowerExp lhs
  (rhsBindings, rhsResult) <- lowerExp rhs
  x <- freshVariable
  let binding = IR.Let x (op lhsResult rhsResult)
  return (lhsBindings ++ rhsBindings ++ [binding], IR.Var x)

-- | Generate a fresh variable.
freshVariable :: State Int IR.Var
freshVariable = do
  i <- get
  put (i + 1)
  return ("x" ++ show i)

-- | Generate a fresh label.
freshLabel :: State Int IR.Var
freshLabel = do
  i <- get
  put (i + 1)
  return ("l" ++ show i)
