{-# LANGUAGE FlexibleInstances #-}

module Haskix.Haskix.Type where

import Haskix.Haskix.Attrib
import Haskix.Haskix.Exts
import Haskix.Types.Module
import Haskix.Types.Name.Occurrence
import Haskix.Types.Name.Reader
import Haskix.Types.Var

data TyVarBndr flag pass
  = UserTyVar flag (LIdP pass)
  | KindedTyVar flag (LIdP pass) (LKind pass)

deriving instance Show flag => Show (TyVarBndr flag HsixPs)

type LTyVarBndr flag pass = XRec pass (TyVarBndr flag pass)

data ForallTeleScope pass
  = ForallVis
      { vis_bndrs :: [LTyVarBndr () pass]
      }
  | ForallInvis
      { invis_bndrs :: [LTyVarBndr Specificity pass]
      }

deriving instance Show (ForallTeleScope HsixPs)

type Context pass = [LType pass]

type LContext pass = XRec pass (Context pass)

newtype FieldOcc pass = FieldOcc
  { foLabel :: XRec pass OccName
  }

deriving instance Show (FieldOcc HsixPs)

type LFieldOcc pass = XRec pass (FieldOcc pass)

data ConDeclField pass = ConDeclField
  { cdFldAttr :: LAttribs pass,
    cdFldVis :: Maybe Visibility,
    cdFldNames :: [LFieldOcc pass],
    cdFldType :: LType pass
  }

deriving instance Show (ConDeclField HsixPs)

type LConDeclField pass = XRec pass (ConDeclField pass)

data Type pass
  = ForallTy
      { tele :: ForallTeleScope pass,
        body :: LType pass
      }
  | QualTy
      { ctxt :: LContext pass,
        body :: LType pass
      }
  | TyVar (LIdP pass)
  | AppTy (LType pass) (LType pass)
  | AppKindTy (LType pass) (LKind pass)
  | FunTy (LType pass) (LType pass)
  | ListTy (LType pass)
  | TupleTy [LType pass]
  | ParTy (LType pass)
  | StarTy
  | KindSig (LType pass) (LKind pass)
  | RecTy [LConDeclField pass]

deriving instance Show (Type HsixPs)

type LType pass = XRec pass (Type pass)

type Kind pass = Type pass

type LKind pass = XRec pass (Kind pass)

newtype LQTyVars pass = LQTyVars
  { qExplicit :: [LTyVarBndr () pass]
  }

deriving instance Show (LQTyVars HsixPs)

data ConDetails tyarg arg rec
  = PrefixCon [tyarg] [arg]
  | RecCon rec
  | InfixCon arg arg

deriving instance (Show tyarg, Show arg, Show rec) => Show (ConDetails tyarg arg rec)

data OuterTyVarBndrs flag pass
  = OuterImplicit
  | OuterExplicit
      { oBndrs :: [LTyVarBndr flag (NoHsixTc pass)]
      }

deriving instance Show flag => Show (OuterTyVarBndrs flag HsixPs)

type OuterSigTyVarBndrs pass = OuterTyVarBndrs Specificity pass

data SigType pass = Sig
  { sigBndrs :: OuterSigTyVarBndrs pass,
    sigBody :: LType pass
  }

deriving instance Show (SigType HsixPs)

type LSigType pass = XRec pass (SigType pass)

newtype WildCardsBndrs pass thing = WC
  { wcBody :: thing
  }
  deriving (Show)

type LWcType pass = WildCardsBndrs pass (LType pass)

type LSigWcType pass = WildCardsBndrs pass (LSigType pass)

newtype PatSigType pass = PS
  { psBody :: LType pass
  }

deriving instance Show (PatSigType HsixPs)

data AmbiguousFieldOcc pass
  = Unambiguous RdrName
  | Ambiguous RdrName
  deriving (Show)

type LAmbiguousFieldOcc pass = XRec pass (AmbiguousFieldOcc pass)