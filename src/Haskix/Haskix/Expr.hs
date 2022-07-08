{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RoleAnnotations #-}

module Haskix.Haskix.Expr where

import Haskix.Haskix.Binds
import Haskix.Haskix.Exts
import Haskix.Haskix.Lit as L
import Haskix.Haskix.Pat
import Haskix.Haskix.Type
import Haskix.Types.Basic
import Haskix.Types.FieldLabel
import Haskix.Types.Fixity
import Haskix.Types.Name.Occurrence

data DoFlavour = DoExpr | ListComp
  deriving (Eq, Show)

data StmtContext pass
  = DoStmt DoFlavour
  | PatGuard (MatchContext pass)

deriving instance Show (StmtContext HsixPs)

data StmtLR idL idR body
  = LastStmt body (Maybe Bool)
  | BindStmt (LPat idL) body
  | BodyStmt body

deriving instance Show body => Show (StmtLR HsixPs HsixPs body)

type LStmt id body = XRec id (StmtLR id id body)

type ExprStmt id = LStmt id (LExpr id)

type GuardLStmt id = LStmt id (LExpr id)

data MatchContext pass
  = FunRHS
      { mcFun :: LIdP pass,
        mcFixity :: LexicalFixity
      }
  | LambdaExpr
  | CaseAlt
  | IfAlt
  | PatBindRHS
  | PatBindGuards
  | RecUpd
  | StmtCtxt (StmtContext pass)

deriving instance Show (MatchContext HsixPs)

data Match pass body = Match
  { mCtxt :: MatchContext pass,
    mPats :: [LPat pass],
    mGRHSS :: GRHSs pass body
  }

deriving instance Show body => Show (Match HsixPs body)

type LMatch pass body = XRec pass (Match pass body)

data MatchGroup pass body = MG
  { mgAlts :: XRec pass [LMatch pass body],
    origin :: Origin
  }

deriving instance Show body => Show (MatchGroup HsixPs body)

type role GRHS nominal nominal

data GRHS pass body = GRHS [GuardLStmt pass] body

deriving instance Show body => Show (GRHS HsixPs body)

type LGRHS pass body = XRec pass (GRHS pass body)

data GRHSs pass body = GRHSs
  { grhssGRHSs :: [LGRHS pass body],
    grhssLocalBinds :: LocalBinds pass
  }

deriving instance Show body => Show (GRHSs HsixPs body)

type RecordBinds pass = RecFields pass (LExpr pass)

newtype DotFieldOcc pass = DotFieldOcc
  { dfoLabel :: XRec pass FieldLabelString
  }

deriving instance Show (DotFieldOcc HsixPs)

newtype FieldLabelStrings pass = FieldLabelStrings [XRec pass (DotFieldOcc pass)]

deriving instance Show (FieldLabelStrings HsixPs)

type LFieldLabelStrings pass = XRec pass (FieldLabelStrings pass)

type RecProj p arg = FieldBind (LFieldLabelStrings p) arg

type RecUpdProj p = RecProj p (LExpr p)

type LRecUpdProj p = XRec p (RecUpdProj p)

data ArithSeqInfo id
  = From (LExpr id)
  | FromThen (LExpr id) (LExpr id)
  | FromTo (LExpr id) (LExpr id)
  | FromThenTo (LExpr id) (LExpr id) (LExpr id)

deriving instance Show (ArithSeqInfo HsixPs)

newtype TupleArg pass = Present (LExpr pass)

deriving instance Show (TupleArg HsixPs)

data Expr pass
  = Var (LIdP pass)
  | UnboundVar OccName
  | RecSel (FieldOcc pass)
  | Lit (L.Lit pass)
  | Lam (MatchGroup pass (LExpr pass))
  | App (LExpr pass) (LExpr pass)
  | AppType (LExpr pass) (LWcType (NoHsixTc pass))
  | OpApp (LExpr pass) (LExpr pass) (LExpr pass)
  | NegApp (LExpr pass)
  | Par (LExpr pass)
  | SectionL (LExpr pass) (LExpr pass)
  | SectionR (LExpr pass) (LExpr pass)
  | ExplicitTuple [TupleArg pass]
  | Case (LExpr pass) (MatchGroup pass (LExpr pass))
  | If (LExpr pass) (LExpr pass) (LExpr pass)
  | MultiIf [LGRHS pass (LExpr pass)]
  | Let (LocalBinds pass) (LExpr pass)
  | Do DoFlavour (XRec pass [ExprStmt pass])
  | ExplicitList [LExpr pass]
  | RecordCon
      { rconCon :: XRec pass (ConLikeP pass),
        rconFlds :: RecordBinds pass
      }
  | RecordUpd
      { rupdExpr :: LExpr pass,
        rupdFlds :: [LRecUpdProj pass]
      }
  | GetField
      { gfExpr :: LExpr pass,
        gfField :: XRec pass (DotFieldOcc pass)
      }
  | Projection
      { projFlds :: [XRec pass (DotFieldOcc pass)]
      }
  | ExprWithTySig (LExpr pass) (LSigWcType (NoHsixTc pass))
  | ArithSeq (ArithSeqInfo pass)

deriving instance Show (Expr HsixPs)

type LExpr pass = XRec pass (Expr pass)