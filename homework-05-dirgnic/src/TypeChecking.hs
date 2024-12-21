module TypeChecking where

import Syntax
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

-- Supported types
data Type = TypeInt | TypeBool
  deriving (Show, Eq)

-- Type errors
data TypeError = TypeMismatch | NotInScope
  deriving (Show, Eq)

type Environment = [(Var, Type)]
type TypeChecking a = ReaderT Environment (Except TypeError) a

-- Type check a statement
typeCheck :: Stm -> Either TypeError Type
typeCheck stm = runExcept (runReaderT (inferStm stm) [])

inferStm :: Stm -> TypeChecking Type
inferStm (Let var expr body) = do
  typ <- inferExp expr
  local ((var, typ) :) (inferStm body)
inferStm (Res expr) = inferExp expr
inferStm (Seq stmts final) = do
  mapM_ inferStm stmts
  inferStm final
inferStm (Ite cond thenStm elseStm) = do
  condType <- inferExp cond
  assertTypeEqual condType TypeBool
  thenType <- inferStm thenStm
  elseType <- inferStm elseStm
  assertTypeEqual thenType elseType
  return thenType

inferExp :: Exp -> TypeChecking Type
inferExp (Num _) = return TypeInt
inferExp (Boo _) = return TypeBool
inferExp (Var x) = do
  env <- ask
  case lookup x env of
    Nothing -> throwTypeError NotInScope
    Just typ -> return typ
inferExp (Add lhs rhs) = do
  assertTypeEqual TypeInt =<< inferExp lhs
  assertTypeEqual TypeInt =<< inferExp rhs
  return TypeInt
inferExp (And lhs rhs) = do
  assertTypeEqual TypeBool =<< inferExp lhs
  assertTypeEqual TypeBool =<< inferExp rhs
  return TypeBool
inferExp (Sma lhs rhs) = do
  assertTypeEqual TypeInt =<< inferExp lhs
  assertTypeEqual TypeInt =<< inferExp rhs
  return TypeBool

assertTypeEqual :: Type -> Type -> TypeChecking ()
assertTypeEqual t1 t2 =
  if t1 == t2
    then return ()
    else throwTypeError TypeMismatch

throwTypeError :: TypeError -> TypeChecking a
throwTypeError = lift . throwE

typeCheckExp :: Exp -> Either String Exp
typeCheckExp (Eq lhs rhs) = do
  l <- typeCheckExp lhs
  r <- typeCheckExp rhs
  case (l, r) of
    (Boo _, Boo _) -> Left "Equality (==) cannot be applied to booleans"
    (Num _, Num _) -> Right (Eq lhs rhs)
    _              -> Left "Operands of == must be of the same type"
typeCheckExp (Var v) = Right (Var v) -- Example case
-- Handle other cases
