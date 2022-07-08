{-# LANGUAGE FlexibleInstances #-}

module Haskix.Haskix.Pat where

import {-# SOURCE #-} Haskix.Haskix.Expr
import Haskix.Haskix.Exts
import Haskix.Haskix.Lit
import Haskix.Haskix.Type
import Haskix.Types.Name.Reader

type family ConLikeP p

type instance ConLikeP HsixPs = RdrName

data FieldBind lhs rhs = FieldBind
  { fbLHS :: lhs,
    fbRHS :: rhs,
    fbPun :: Bool
  }

deriving instance (Show lhs, Show rhs) => Show (FieldBind lhs rhs)

type RecField pass arg = FieldBind (LFieldOcc pass) arg

type LRecField pass arg = XRec pass (RecField pass arg)

type RecUpdField pass = FieldBind (LAmbiguousFieldOcc pass) (LExpr pass)

type LRecUpdField pass = XRec pass (RecUpdField pass)

data RecFields pass args = RecFields
  { recFlds :: [LRecField pass args],
    recDotDot :: Maybe ()
  }

deriving instance Show args => Show (RecFields HsixPs args)

type ConPatDetails pass = ConDetails (PatSigType (NoHsixTc pass)) (LPat pass) (RecFields pass (LPat pass))

data Pat pass
  = WildPat
  | VarPat (LIdP pass)
  | AsPat (LIdP pass) (LPat pass)
  | ParPat (LPat pass)
  | ListPat [LPat pass]
  | TuplePat [LPat pass]
  | ConPat
      { patCon :: XRec pass (ConLikeP pass),
        patArgs :: ConPatDetails pass
      }
  | LitPat (Lit pass)

isVarPatRec :: Pat a -> Bool
isVarPatRec p =
  case p of
    VarPat _ -> True
    ParPat (XRec p) -> isVarPatRec p
    _ -> False

deriving instance Show (Pat HsixPs)

type LPat pass = XRec pass (Pat pass)