module TypeChecking (
  infer,        -- Export the `infer` function
  Type(..),     -- Export the `Type` type
  TypeError(..),
  Exp(..)-- Export the `TypeError` type and its constructors
) where

import qualified Data.Map as Map

-- Define the Type data type
data Type = TInt | TBool
  deriving (Show, Eq)

-- Define the TypeError data type for type checking errors
data TypeError = NotInScope | TypeMismatch
  deriving (Show, Eq)

-- Define the expression data type
data Exp
  = IntLit Int
  | BoolLit Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Not Exp
  | Eq Exp Exp
  | Var String
  | Let String Exp Exp
  deriving (Show)

-- Type environment
type TypeEnv = Map.Map String Type

-- Function to infer the type of an expression
infer :: TypeEnv -> Exp -> Either String Type
infer _ (IntLit _) = Right TInt
infer _ (BoolLit _) = Right TBool
infer env (Add e1 e2) = checkBinaryOp TInt env e1 e2
infer env (Sub e1 e2) = checkBinaryOp TInt env e1 e2
infer env (Not e) =
  case infer env e of
    Right TBool -> Right TBool
    Right t     -> Left $ "Expected TBool, but got " ++ show t
    Left err    -> Left err
infer env (Eq e1 e2) =
  case (infer env e1, infer env e2) of
    (Right t1, Right t2) | t1 == t2 -> Right TBool
    (Right _, Right _) -> Left "Operands must have the same type"
    (Left err, _) -> Left err
    (_, Left err) -> Left err
infer env (Var x) =
  case Map.lookup x env of
    Just t -> Right t
    Nothing -> Left $ "Variable not found: " ++ x
infer env (Let x e1 e2) = do
  t1 <- infer env e1
  infer (Map.insert x t1 env) e2

-- Helper function to check binary operations
checkBinaryOp :: Type -> TypeEnv -> Exp -> Exp -> Either String Type
checkBinaryOp expected env e1 e2 = do
  t1 <- infer env e1
  t2 <- infer env e2
  if t1 == expected && t2 == expected
    then Right expected
    else Left $ "Expected both operands to be " ++ show expected
