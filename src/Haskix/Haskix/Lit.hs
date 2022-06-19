module Haskix.Haskix.Lit where

data IntegralLit = IL
  { ilText :: String,
    ilNeg :: Bool,
    ilVal :: Integer
  }
  deriving (Show)

data FractionalLit = FL
  { flText :: String,
    flNeg :: Bool,
    flVal :: Rational
  }
  deriving (Show)

data Lit pass
  = HsixChar Char
  | HsixString String
  | HsixInt IntegralLit
  | HsixRat FractionalLit
  deriving (Show)