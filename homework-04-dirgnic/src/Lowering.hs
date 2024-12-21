module Lowering where

import Syntax (
  Stm(..), Exp(..))

import IntermediateRepresentation (
  Program, JumpTarget(JumpTarget), Block(Block))
import qualified IntermediateRepresentation as IR (
  Var, Label, Binding(Let), App(..), Val(..), Jump(..))

import Control.Monad.Trans.State (
  State, get, put, evalState)


-- | Lower an expression from the source language to the intermediate
-- representation.
lower :: Stm -> Program
lower statement = do
  let returnJumpTarget = JumpTarget "return" ["result"] (Block [] (IR.End (IR.Var "result")))
  let (entryBlock, program) = evalState (lowerStm statement "return") 0
  (JumpTarget "entrypoint" [] entryBlock : returnJumpTarget : program)


lowerStm :: Stm -> IR.Label -> State Int (Block, [JumpTarget])
lowerStm (Res result) continuation = do
  (bindings, trivial) <- lowerExp result
  return (Block bindings (IR.Jwa continuation [trivial]), [])
lowerStm (Let x rhs body) continuation = do
  bodyLabel <- freshLabel
  (rhsBlock, rhsProgram) <- lowerStm rhs bodyLabel
  (bodyBlock, bodyProgram) <- lowerStm body continuation
  let bodyJumpTarget = JumpTarget bodyLabel [x] bodyBlock
  return (rhsBlock, rhsProgram ++ [bodyJumpTarget] ++ bodyProgram)
lowerStm (Ite condition thenStm elseStm) continuation = do
  (conditionBindings, conditionResult) <- lowerExp condition
  (thenBlock, thenProgram) <- lowerStm thenStm continuation
  (elseBlock, elseProgram) <- lowerStm elseStm continuation
  thenLabel <- freshLabel
  elseLabel <- freshLabel
  let conditionBlock = Block conditionBindings (IR.Bwa conditionResult thenLabel [] elseLabel [])
  let thenJumpTarget = JumpTarget thenLabel [] thenBlock
  let elseJumpTarget = JumpTarget elseLabel [] elseBlock
  return (conditionBlock, thenJumpTarget : elseJumpTarget : thenProgram ++ elseProgram)
lowerStm (For iterations variable initial body) continuation = do
  startLabel <- freshLabel
  loopLabel <- freshLabel
  endLabel <- freshLabel

  (iterBindings, iterVal) <- lowerExp iterations
  (initBindings, initVal) <- lowerExp initial
  (bodyBlock, bodyProgram) <- lowerStm body loopLabel

  let loopBlock = Block (initBindings ++ [IR.Let variable (IR.Add initVal iterVal)]) 
                        (IR.Jwa loopLabel [])
  let endJump = JumpTarget endLabel [] (Block [] (IR.Jwa continuation []))

  return (Block iterBindings (IR.Jwa startLabel []), [JumpTarget startLabel [] loopBlock] ++ bodyProgram ++ [endJump])


-- | Lower an expression from the source language to the intermediate
-- representation using a state of int for fresh name generation.
lowerExp :: Exp -> State Int ([IR.Binding], IR.Val)
lowerExp (Var x) = do
  return ([], IR.Var x)
lowerExp (Num n) = do
  return ([], IR.Num n)
lowerExp (Sma lhs rhs) = do
  (lhsBindings, lhsResult) <- lowerExp lhs
  (rhsBindings, rhsResult) <- lowerExp rhs
  x <- freshVariable
  let binding = IR.Let x (IR.Sma lhsResult rhsResult)
  let bindings = lhsBindings ++ rhsBindings ++ [binding]
  let result = IR.Var x
  return (bindings, result)
lowerExp (Syntax.Boo b) = do
  return ([], IR.Boo b)
lowerExp (Syntax.And lhs rhs) = do
  (lhsBindings, lhsResult) <- lowerExp lhs
  (rhsBindings, rhsResult) <- lowerExp rhs
  x <- freshVariable
  let binding = IR.Let x (IR.And lhsResult rhsResult)
  let bindings = lhsBindings ++ rhsBindings ++ [binding]
  let result = IR.Var x
  return (bindings, result)
lowerExp (Syntax.Add lhs rhs) = do
  (lhsBindings, lhsResult) <- lowerExp lhs
  (rhsBindings, rhsResult) <- lowerExp rhs
  x <- freshVariable
  let binding = IR.Let x (IR.Add lhsResult rhsResult)
  let bindings = lhsBindings ++ rhsBindings ++ [binding]
  let result = IR.Var x
  return (bindings, result)


-- | Generate a fresh variable.
freshVariable :: State Int IR.Var
freshVariable = do
  i <- get
  put (i + 1)
  return ("x" ++ show i)

-- | Generate a fresh label.
freshLabel :: State Int IR.Label
freshLabel = do
  i <- get
  put (i + 1)
  return ("l" ++ show i)
