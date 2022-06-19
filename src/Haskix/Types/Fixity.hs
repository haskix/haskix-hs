module Haskix.Types.Fixity where

data LexicalFixity = Prefix | Infix
  deriving (Show)

data Assoc = LeftAssoc | RightAssoc | NoneAssoc
  deriving (Show)