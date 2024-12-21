module IntermediateRepresentation (
  ANF(..),      -- Export ANF with all its constructors
  Stm(..),      -- Export Stm with all its constructors
  App(..),      -- Export App with all its constructors
  Val(..),      -- Export Val with all its constructors
  Var           -- Export Var (type alias for String)
) where
  
type Var = String

data ANF =
  Block [Stm] Val
   deriving (Show, Eq)

data Stm =
  Let Var App
    deriving (Show, Eq)

data App =
  Add Val Val |
  And Val Val |
  Sma Val Val |
  Sub Val Val |  -- IR for subtraction
  Not Val |      -- IR for negation
  Eq Val Val     -- IR for equality
    deriving (Show, Eq)

data Val =
  Num Integer |
  Boo Bool |
  Var Var
    deriving (Show, Eq)



