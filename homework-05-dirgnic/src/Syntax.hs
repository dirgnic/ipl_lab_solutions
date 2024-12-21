module Syntax where
import Data.List (intercalate)
type Var = String
-- Statements

data Stm
  = Let Var Exp Stm    -- Variable declaration
  | Res Exp            -- Return statement
  | Ite Exp Stm Stm    -- If-then-else
  | Seq [Stm] Stm      -- Sequence of statements
  deriving (Show, Eq)

-- Expressions

data Exp
  = Num Integer        -- Numeric literal
  | Boo Bool           -- Boolean literal
  | Var Var            -- Variable
  | Add Exp Exp        -- Addition
  | And Exp Exp        -- Logical AND
  | Sma Exp Exp        -- Less-than comparison
  | Eq Exp Exp         -- Equality comparison
  deriving (Show, Eq)

