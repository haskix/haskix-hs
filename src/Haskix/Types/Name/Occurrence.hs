module Haskix.Types.Name.Occurrence where

newtype OccName = OccName
  { occNName :: String
  }
  deriving (Eq, Show)