{-# LANGUAGE FlexibleInstances #-}

module Haskix.Haskix.Attrib where

import {-# SOURCE #-} Haskix.Haskix.Expr
import Haskix.Haskix.Exts

data Attrib pass = Attrib
  { attrName :: String,
    attrVal :: LExpr pass
  }

deriving instance Show (Attrib HsixPs)

type LAttrib pass = XRec pass (Attrib pass)

type LAttribs pass = [LAttrib pass]