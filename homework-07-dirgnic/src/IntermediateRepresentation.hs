module IntermediateRepresentation where

-- | Labels
type Lab = String

-- | Variables
type Var = String

-- | References (pointers)
type Ref = String

-- | A program in our intermeditate representation.
type Program = [JoinPoint]

-- | The target of a jump.
data JoinPoint = JoinPoint Lab Block
  deriving (Show, Eq)

-- | A block is a list of bindings and ends in a jump.
data Block =
  Block [Stm] Jump
   deriving (Show, Eq)

data Stm =
  Let Var App
    deriving (Show, Eq)

data App =
  Add Val Val |
  And Val Val |
  Sma Val Val |
  Alo Ref |
  Loa Ref |
  Sto Ref Val |
  Mal Ref
    deriving (Show, Eq)

data Val =
  Num Integer |
  Boo Bool |
  Var Var
    deriving (Show, Eq)

-- | Every block ends with a jump.
data Jump =
  End Val |
  -- ^ end computation with result
  Jum Lab |
  -- ^ jump with argument
  Bra Val Lab Lab
  -- ^ branch with argument
    deriving (Show, Eq)

