module Syntax (Exp(..)) where

type Var = String

data Exp =
  IntLit Integer |
  BoolLit Bool |
  Boo Bool |
  And Exp Exp |
  Num Integer |
  Add Exp Exp |
  Sma Exp Exp |
  Let Var Exp Exp |
  Sub Exp Exp |  -- Integer subtraction
  Not Exp |      -- Boolean negation
  Eq Exp Exp |    -- Optional: Equality comparison
  Var Var
   deriving (Show, Eq)

