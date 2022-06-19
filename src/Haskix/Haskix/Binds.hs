{-# LANGUAGE FlexibleInstances #-}

module Haskix.Haskix.Binds where

import {-# SOURCE #-} Haskix.Haskix.Expr
import Haskix.Haskix.Exts
import Haskix.Haskix.Pat
import Haskix.Haskix.Type

data FixitySig pass = FixitySig
  { fsId :: [LIdP pass],
    fsPrecGroup :: LIdP pass
  }

deriving instance Show (FixitySig HsixPs)

data Sig pass
  = TypeSig [LIdP pass] (LSigWcType pass)
  | ClassOpSig [LIdP pass] (LSigType pass)
  | FixSig (FixitySig pass)

deriving instance Show (Sig HsixPs)

type LSig pass = XRec pass (Sig pass)

data BindLR idL idR
  = FunBind
      { funId :: LIdP idL,
        funMatches :: MatchGroup idR (LExpr idR)
      }
  | PatBind
      { patLhs :: LPat idL,
        patRhs :: GRHSs idR (LExpr idR)
      }
  | VarBind
      { varId :: LIdP idL,
        varRhs :: LExpr idR
      }

deriving instance Show (BindLR HsixPs HsixPs)

type Bind id = BindLR id id

type LBindLR idL idR = XRec idL (BindLR idL idR)

type LBindsLR idL idR = [LBindLR idL idR]

type LBinds id = LBindsLR id id

data ValBindsLR idL idR = ValBinds (LBindsLR idL idR) [LSig idR]

deriving instance Show (ValBindsLR HsixPs HsixPs)

data LocalBindsLR idL idR
  = LocalValBinds (ValBindsLR idL idR)
  | EmptyLocalBinds

deriving instance Show (LocalBindsLR HsixPs HsixPs)

type LocalBinds id = LocalBindsLR id id