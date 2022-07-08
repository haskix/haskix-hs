module Haskix.Haskix.Lit where

data IntegralLit = IL
  { ilVal :: Integer,
    ilText :: String,
    ilNeg :: Bool
  }
  deriving (Show, Eq, Ord)

data FractionalLit = FL
  { flVal :: Rational,
    flText :: String,
    flNeg :: Bool
  }
  deriving (Show, Eq, Ord)

data Lit pass
  = HsixChar Char
  | HsixString String
  | HsixInt IntegralLit
  | HsixRat FractionalLit
  deriving (Show)