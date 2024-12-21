-- TypeChecking.hs
module TypeChecking where

import Syntax (
  Stm(..), Exp(..), Var)

import Control.Monad.Trans.Class (
  lift)
import Control.Monad.Trans.Reader (
  ReaderT, ask, runReaderT, local)
import Control.Monad.Trans.Except (
  Except, throwE, runExcept)


data Type = TypeInt | TypeBool
  deriving (Show, Eq)

data TypeError = TypeMismatch | NotInScope
  deriving (Show, Eq)

type Environment = [(Var, Type)]

-- | The empty environment.
emptyEnvironment :: Environment
emptyEnvironment = []


-- | Type check the given expression. A result of 'Nothing' means everything
-- is ok.
typeCheck :: Stm -> Maybe TypeError
typeCheck statement =
  case runExcept (runReaderT (inferStm statement) emptyEnvironment) of
    Left typeError -> Just typeError
    Right _ -> Nothing

inferStm :: Stm -> TypeChecking Type
inferStm (Let x rhs body) = do
  typ <- inferStm rhs
  withLocalVariableType x typ (inferStm body)
inferStm (Res result) = do
  typ <- inferExp result
  return typ
inferStm (Ite condition thenBranch elseBranch) = do
  conditionType <- inferExp condition
  assertTypeEqual conditionType TypeBool
  thenType <- inferStm thenBranch
  elseType <- inferStm elseBranch
  assertTypeEqual thenType elseType
  return thenType
inferStm (For iterations variable initial body) = do
  iterType <- inferExp iterations
  assertTypeEqual iterType TypeInt
  initType <- inferExp initial
  _ <- withLocalVariableType variable initType (inferStm body)
  return initType


-- | Type check the given expression, either returning its type or throwing a
-- type error.
inferExp :: Exp -> TypeChecking Type
inferExp (Boo _) = do
  return TypeBool
inferExp (And lhs rhs) = do
  lhsType <- inferExp lhs
  rhsType <- inferExp rhs
  assertTypeEqual lhsType TypeBool
  assertTypeEqual rhsType TypeBool
  return TypeBool
inferExp (Num _) = do
  return TypeInt
inferExp (Add lhs rhs) = do
  lhsType <- inferExp lhs
  rhsType <- inferExp rhs
  assertTypeEqual lhsType TypeInt
  assertTypeEqual rhsType TypeInt
  return TypeInt
inferExp (Sma lhs rhs) = do
  lhsType <- inferExp lhs
  rhsType <- inferExp rhs
  assertTypeEqual lhsType TypeInt
  assertTypeEqual rhsType TypeInt
  return TypeBool
inferExp (Var x) = do
  environment <- askEnvironment
  case lookup x environment of
    Nothing -> do
      throwTypeError NotInScope
    Just typ -> do
      return typ

type TypeChecking a = ReaderT Environment (Except TypeError) a

assertTypeEqual :: Type -> Type -> TypeChecking ()
assertTypeEqual typ1 typ2 = do
  if typ1 == typ2
    then do
      return ()
    else do
      throwTypeError TypeMismatch

throwTypeError :: TypeError -> TypeChecking a
throwTypeError e = lift (throwE e)

askEnvironment :: TypeChecking Environment
askEnvironment = ask

withLocalVariableType :: Var -> Type -> TypeChecking a -> TypeChecking a
withLocalVariableType x t m = local (\e -> (x, t) : e) m

---
