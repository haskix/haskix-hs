{-# LANGUAGE LambdaCase #-}

module Haskix.Parser where

import Control.Monad (void, when)
import Control.Monad.State (State, modify, runState)
import Data.List (singleton)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set as Set (empty)
import Data.Void (Void)
import Haskix.Haskix
import Haskix.Haskix.Attrib
import Haskix.Haskix.Binds
import Haskix.Haskix.Decls as Hsix
import Haskix.Haskix.Expr as E
import Haskix.Haskix.Exts
import Haskix.Haskix.Lit
import Haskix.Haskix.Pat
import Haskix.Haskix.Token
import Haskix.Haskix.Type
import Haskix.Types.Basic
import Haskix.Types.Fixity as Fix
import Haskix.Types.Module
import Haskix.Types.Name
import Haskix.Types.Name.Occurrence
import Haskix.Types.Name.Reader
import Haskix.Types.Var
import Text.Megaparsec hiding (State, Token, token)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char.Lexer as L

newtype ParserState = ParserState
  { subMod :: [String]
  }
  deriving (Show)

type Parser a = ParsecT Void [Token] (State ParserState) a

(~>) :: Parser a -> Parser b -> Parser b
l ~> v = lookAhead l >> v

keyword :: TokenKind -> Parser ()
keyword k = P.token (\x -> if woVal x == k then Just () else Nothing) Set.empty

optKeyword :: TokenKind -> Parser ()
optKeyword k = void (optional $ keyword k)

token :: (TokenKind -> Maybe a) -> Parser a
token pred = P.token (pred . woVal) Set.empty

inBrace :: Parser a -> Parser a
inBrace = between (keyword TkLeftBrace) (keyword TkRightBrace)

inBracket :: Parser a -> Parser a
inBracket = between (keyword TkLeftBracket) (keyword TkRightBracket)

inParen :: Parser a -> Parser a
inParen = between (keyword TkLeftParen) (keyword TkRightParen)

inBacktick :: Parser a -> Parser a
inBacktick = between (keyword TkBacktick) (keyword TkBacktick)

withSemi :: Parser a -> Parser a
withSemi p = p <* semicolon

sepByComma :: Parser a -> Parser [a]
sepByComma p = sepBy p (keyword TkComma)

sepByComma1 :: Parser a -> Parser [a]
sepByComma1 p = sepBy1 p (keyword TkComma)

optWhere :: Parser ()
optWhere = optKeyword TkWhere

upperName :: Parser String
upperName =
  token
    ( \case
        TkUpperName n -> Just n
        _ -> Nothing
    )

lowerName :: Parser String
lowerName =
  token
    ( \case
        TkLowerName n -> Just n
        _ -> Nothing
    )

name :: Parser String
name =
  token
    ( \case
        TkLowerName n -> Just n
        TkUpperName n -> Just n
        _ -> Nothing
    )

varSymbol :: Parser String
varSymbol =
  token
    ( \case
        TkVarSymbol n -> Just n
        TkConstructorSymbol n -> Just n
        _ -> Nothing
    )

conSymbol :: Parser String
conSymbol =
  token
    ( \case
        TkConstructorSymbol n -> Just n
        _ -> Nothing
    )

symbol :: Parser String
symbol =
  token
    ( \case
        TkVarSymbol n -> Just n
        TkConstructorSymbol n -> Just n
        _ -> Nothing
    )

dot :: Parser ()
dot =
  choice
    [ keyword TkTightInfixProj,
      keyword TkPrefixProj,
      keyword (TkVarSymbol ".")
    ]

semicolon :: Parser ()
semicolon = keyword TkSemicolon

colon :: Parser ()
colon = keyword TkColon

equal :: Parser ()
equal = keyword TkEqual

predIdP :: Parser String -> Parser (LIdP HsixPs)
predIdP p = do
  qual <- optional modulePath
  val <- p
  return
    ( XRec
        ( case qual of
            Just q -> Qual q (OccName val)
            Nothing -> Unqual (OccName val)
        )
    )

idP :: Parser (LIdP HsixPs)
idP =
  predIdP
    ( choice
        [ name,
          inParen symbol,
          inBacktick name
        ]
    )

lowerNameIdP :: Parser (LIdP HsixPs)
lowerNameIdP = predIdP lowerName

upperNameIdP :: Parser (LIdP HsixPs)
upperNameIdP = predIdP upperName

conSymIdP :: Parser (LIdP HsixPs)
conSymIdP = predIdP conSymbol

conIdP = predIdP (upperName <|> inParen conSymbol)

varIdP = predIdP (lowerName <|> inParen varSymbol)

varOpP = predIdP (varSymbol <|> inBacktick lowerName)

modulePathComp :: Parser ModPathComp
modulePathComp =
  choice
    [ MpSuper <$ keyword TkSuper,
      MpSelf <$ keyword TkSelf,
      MpName <$> upperName
    ]

modulePath :: Parser ModulePath
modulePath =
  (keyword TkFlake >> keyword TkTightInfixProj >> ((\ps -> ModulePath {mpFlake = True, mpPath = ps}) <$> rest True))
    <|> ( (\ps -> ModulePath {mpFlake = False, mpPath = ps}) <$> rest False
        )
  where
    rest :: Bool -> Parser [ModPathComp]
    rest allowEmpty =
      (if allowEmpty then many else some) $
        try
          (modulePathComp <* dot)

attr :: TokenKind -> Parser (LAttribs HsixPs)
attr open =
  many $
    withSemi
      ( between
          (keyword open)
          (keyword TkCloseAttr)
          ( do
              n <- lowerName
              v <- lExpr
              return (XRec Attrib {attrName = n, attrVal = v})
          )
      )

attribute :: Parser (LAttribs HsixPs)
attribute = attr TkOpenAttr

modAttribute :: Parser (LAttribs HsixPs)
modAttribute = attr TkGlobalAttr

visibility :: Parser (Maybe Visibility)
visibility =
  optional $
    keyword TkPrivate
      >> ( inParen
             ( choice
                 [ VisSelf <$ keyword TkSelf,
                   VisSuper <$ keyword TkSuper,
                   VisFlake <$ keyword TkFlake,
                   keyword TkIn >> VisPath
                     <$> ( do
                             prefix <- modulePath
                             comp <- modulePathComp
                             return prefix {mpPath = mpPath prefix ++ [comp]}
                         )
                 ]
             )
             <|> return VisSelf
         )

openDecl :: LAttribs HsixPs -> Parser (OpenDecl HsixPs)
openDecl attr = do
  keyword TkOpen
  withSemi
    ( ( keyword TkFlake >> ((\ch -> OpenDecl {odAttr = attr, odTree = OtrFlake ch}) <$> child)
      )
        <|> ((\tr -> OpenDecl {odAttr = attr, odTree = OtrOther tr}) <$> tree)
    )
  where
    tree :: Parser (OpenTree HsixPs)
    tree =
      choice
        [ keyword TkSelf >> (OtSelf <$> child),
          keyword TkSuper >> (OtSuper <$> child),
          do
            (ch, name) <-
              token
                ( \case
                    TkUpperName x -> Just (True, x)
                    TkLowerName x -> Just (False, x)
                    TkConstructorSymbol x -> Just (False, x)
                    TkVarSymbol x -> Just (False, x)
                    _ -> Nothing
                )
            if ch
              then OtName name <$> child
              else return (OtName name [])
        ]
    child :: Parser [OpenTree HsixPs]
    child =
      ( dot
          >> choice
            [ keyword TkUsing >> inBrace (endBy tree semicolon),
              keyword TkLeftBrace >> (endBy tree semicolon <* keyword TkRightBrace),
              singleton <$> tree
            ]
      )
        <|> return []

moduleBody :: Parser (ModBody HsixPs)
moduleBody = do
  attrs <- modAttribute
  topDecls <- many topDecl
  return ModBody {mdbAttr = attrs, mdbBody = XRec <$> topDecls}

moduleDecl :: LAttribs HsixPs -> Parser (ModDecl HsixPs)
moduleDecl attrib = do
  keyword TkModule
  name <- upperName
  body <-
    optional $
      optWhere >> inBrace moduleBody
  when (isNothing body) $
    modify (\s -> s {subMod = name : subMod s})
  semicolon
  return
    ModDecl
      { modName = name,
        modAttrib = attrib,
        Hsix.modBody = body
      }

precedenceGroupDecl :: Parser (PrecGrpDecl HsixPs)
precedenceGroupDecl = do
  keyword TkPrecedence
  name <- lowerName ~> idP
  optWhere
  (above, below, assoc) <-
    inBrace $ do
      let field name body =
            optional $
              withSemi (keyword (TkLowerName name) >> keyword TkEqual >> body)
      let precList name =
            fromMaybe []
              <$> field name (inBracket (sepBy idP (keyword TkComma)))
      above <- precList "above"
      below <- precList "below"
      assoc <-
        field
          "assoc"
          ( choice
              [ LeftAssoc <$ keyword (TkUpperName "Left"),
                RightAssoc <$ keyword (TkUpperName "Right"),
                NoneAssoc <$ keyword (TkUpperName "None")
              ]
          )
      return (above, below, assoc)

  semicolon
  return
    PrecGrpDecl
      { pgdName = name,
        pgdAbove = above,
        pgdBelow = below,
        pgdAssoc = assoc
      }

tyVarInTyVarBndr :: a -> Parser (TyVarBndr a HsixPs)
tyVarInTyVarBndr v = do
  name <- lowerNameIdP
  (KindedTyVar v name <$> kindSig)
    <|> return (UserTyVar v name)

lVisTyVarBndr :: Parser (LTyVarBndr () HsixPs)
lVisTyVarBndr =
  XRec
    <$> ( inParen (tyVarInTyVarBndr ())
            <|> (UserTyVar () <$> lowerNameIdP)
        )

lInvisTyVarBndr :: Parser (LTyVarBndr Specificity HsixPs)
lInvisTyVarBndr =
  XRec
    <$> choice
      [ inParen (tyVarInTyVarBndr SpecInferred),
        inBrace (tyVarInTyVarBndr SpecSpecified),
        UserTyVar SpecInferred <$> lowerNameIdP
      ]

forallTele :: Parser (ForallTeleScope HsixPs)
forallTele = do
  keyword TkForall
  bndr <- many lInvisTyVarBndr
  ( ForallInvis bndr <$ dot
    )
    <|> ( ForallVis
            ( ( \case
                  XRec (UserTyVar _ n) -> XRec $ UserTyVar () n
                  XRec (KindedTyVar _ n k) -> XRec $ KindedTyVar () n k
              )
                <$> bndr
            )
            <$ keyword TkArrow
        )

context :: Parser (LContext HsixPs)
context = do
  ctxt <- basicType
  keyword TkFatArrow
  return
    ( case ctxt of
        XRec (TupleTy ctx) -> XRec ctx
        t -> XRec [t]
    )

forallType :: Parser (LType HsixPs)
forallType = do
  tele <- optional forallTele
  typ <- ctxtType
  return
    ( case tele of
        Just t ->
          XRec (ForallTy {tele = t, body = typ})
        Nothing -> typ
    )

forallTypeWithKind :: Parser (LType HsixPs)
forallTypeWithKind = do
  ty <- forallType
  ( \case
      Just p -> XRec (KindSig ty p)
      Nothing -> ty
    )
    <$> optional kindSig

ctxtType :: Parser (LType HsixPs)
ctxtType =
  try
    ( do
        ctx <- context
        typ <- lType
        return $
          XRec (QualTy {ctxt = ctx, body = typ})
    )
    <|> lType

lType :: Parser (LType HsixPs)
lType =
  ( \case
      [x] -> x
      xs@(_ : _) -> foldr1 (\cur acc -> XRec (FunTy cur acc)) xs
  )
    <$> ( basicType
            >>= ( \bt ->
                    (bt :)
                      <$> many (keyword TkArrow >> forallType)
                )
        )

basicType :: Parser (LType HsixPs)
basicType =
  ( \case
      [x] -> x
      xs@(_ : _) -> foldl1 (\prev cur -> XRec $ AppTy prev cur) xs
  )
    <$> sepBy1 argType (void (optional (keyword TkPrefixAt)))

recordBody :: Parser [LConDeclField HsixPs]
recordBody =
  many field
  where
    field :: Parser (LConDeclField HsixPs)
    field =
      do
        attr <- attribute
        vis <- visibility
        names <-
          sepByComma1
            ((\n -> XRec (FieldOcc {foLabel = XRec (OccName n)})) <$> lowerName)
        colon
        typ <- forallType
        semicolon
        return $
          XRec
            ( ConDeclField
                { cdFldAttr = attr,
                  cdFldVis = vis,
                  cdFldNames = names,
                  cdFldType = typ
                }
            )

argType :: Parser (LType HsixPs)
argType =
  XRec
    <$> choice
      [ StarTy <$ keyword TkStar,
        keyword TkLeftParen
          >> ( ( \case
                   [] -> mkBuiltIn "()" ["Data", "Unit"]
                   [x] -> ParTy x
                   xs -> TupleTy xs
               )
                 <$> (sepByComma forallTypeWithKind <* keyword TkRightParen)
             ),
        keyword TkLeftBracket
          >> ( (mkBuiltIn "[]" ["Data", "List"] <$ keyword TkRightBracket)
                 <|> (ListTy <$> (forallTypeWithKind <* keyword TkRightBracket))
             ),
        keyword TkWith >> (RecTy <$> recordBody),
        keyword TkLeftBrace ~> (RecTy <$> inBrace recordBody),
        TyVar <$> ((lowerName <|> upperName) ~> idP)
      ]
  where
    mkBuiltIn name path = TyVar (XRec (Exact $ mkBuiltInName name path))

lKind :: Parser (LKind HsixPs)
lKind = lType

kindSig :: Parser (LKind HsixPs)
kindSig = colon >> lKind

qTyVars :: Parser (LQTyVars HsixPs)
qTyVars = LQTyVars <$> many lVisTyVarBndr

conDecl :: Parser (LConDecl HsixPs)
conDecl = do
  keyword TkBar
  attr <- attribute
  vis <- visibility
  name <- upperNameIdP
  conExTv <-
    optional
      ( keyword TkForall >> many lInvisTyVarBndr
      )
  conCtxt <- optional $ try context
  conArgs <-
    choice
      [ (keyword TkWith <|> keyword TkLeftBrace)
          ~> ( optKeyword TkWith
                 >> (RecCon . XRec <$> inBrace recordBody)
             ),
        PrefixCon [] <$> many argType
      ]
  semicolon
  return
    ( XRec
        ConDeclADT
          { conAttrib = attr,
            conVis = vis,
            conName = name,
            conForall = isJust conExTv,
            conExTv = fromMaybe [] conExTv,
            conCtxt = conCtxt,
            conArgs = conArgs
          }
    )

dataDecl :: LAttribs HsixPs -> Parser (TyClDecl HsixPs)
dataDecl attr =
  do
    name <- upperNameIdP
    qty <- qTyVars
    defn <- do
      optWhere
      (con, derivs) <-
        inBrace $ do
          con <-
            (keyword TkBar ~> (DcConDecl <$> many conDecl))
              <|> (DcRecDecl <$> recordBody)
          drv <- many $ do
            keyword TkDeriving
            types <-
              inParen (sepByComma lType)
                <|> (singleton <$> lType)
            semicolon
            return (XRec DerivingClause {derivClauseTys = types})
          return (con, drv)
      return
        DataDefn
          { ddCon = con,
            ddDerivs = derivs
          }

    semicolon
    return
      DataDecl
        { tcdAttrib = attr,
          tcdLName = name,
          tcdTyVars = qty,
          tcdDataDefn = defn
        }

tyClDecl :: LAttribs HsixPs -> Parser (TyClDecl HsixPs)
tyClDecl attr =
  choice
    [ keyword TkType
        >> withSemi
          ( do
              name <- upperNameIdP
              qtyvars <- qTyVars
              keyword TkEqual
              typ <- forallTypeWithKind
              return
                SynDecl
                  { tcdAttrib = attr,
                    tcdLName = name,
                    tcdTyVars = qtyvars,
                    tcdRHS = typ
                  }
          ),
      keyword TkData >> dataDecl attr,
      keyword TkClass
        >> withSemi
          ( do
              ctxt <- optional $ try context
              name <- conIdP
              tv <- qTyVars
              optWhere
              (bds, sigs) <- inBrace $ bindAndSig classOpSig
              return
                ClassDecl
                  { tcdAttrib = attr,
                    tcdCtxt = ctxt,
                    tcdLName = name,
                    tcdTyVars = tv,
                    tcdSigs = sigs,
                    tcdMeths = bds
                  }
          )
    ]

lit :: Parser (Lit HsixPs)
lit =
  token
    ( \case
        TkChar ch -> Just $ HsixChar ch
        TkString s -> Just (HsixString s)
        TkInteger il -> Just (HsixInt il)
        TkRational rl -> Just (HsixRat rl)
        _ -> Nothing
    )

lFieldOcc :: Parser (LFieldOcc HsixPs)
lFieldOcc = (\n -> XRec (FieldOcc (XRec $ OccName n))) <$> lowerName

fieldBind :: Parser a -> b -> Parser b -> Parser (FieldBind a b)
fieldBind l defR r =
  do
    lhs <- l
    ( keyword TkEqual
        >> ( ( \rh ->
                 FieldBind {fbLHS = lhs, fbRHS = rh, fbPun = False}
             )
               <$> r
           )
      )
      <|> return FieldBind {fbLHS = lhs, fbRHS = defR, fbPun = True}

lRecField :: b -> Parser b -> Parser (LRecField HsixPs b)
lRecField defR r = XRec <$> fieldBind lFieldOcc defR r

recFields :: b -> Parser b -> Parser (RecFields HsixPs b)
recFields defR r =
  inBrace $
    many (withSemi (lRecField defR r))
      >>= ( \flds ->
              ( RecFields {recFlds = flds, recDotDot = Just ()}
                  <$ withSemi (keyword TkDotDot)
              )
                <|> return RecFields {recFlds = flds, recDotDot = Nothing}
          )

punRight = XRec $ Unqual (OccName "pun-right-hand-side")

lPat :: Parser (LPat HsixPs)
lPat =
  do
    p1 <- noInfixCon
    ( do
        con <- conSymIdP <|> inBacktick upperNameIdP
        p2 <- noInfixCon
        return
          ( XRec
              ( ConPat
                  { patCon = con,
                    patArgs = InfixCon p1 p2
                  }
              )
          )
      )
      <|> return p1
  where
    noInfixCon =
      XRec <$> do
        choice
          [ WildPat <$ keyword TkUnderline,
            do
              name <- try varIdP
              ( keyword TkTightInfixAt
                  >> (AsPat name <$> lPat)
                )
                <|> return (VarPat name),
            do
              con <- try conIdP
              let recCon =
                    (\r -> ConPat {patCon = con, patArgs = RecCon r})
                      <$> recFields
                        (XRec $ VarPat punRight)
                        lPat
              choice
                [ keyword TkWith >> recCon,
                  keyword TkLeftBrace ~> recCon,
                  (\ps -> ConPat {patCon = con, patArgs = PrefixCon [] ps})
                    <$> many lPat
                ],
            keyword TkLeftParen
              ~> ( ( \case
                       [x] -> ParPat x
                       v -> TuplePat v
                   )
                     <$> inParen (sepByComma lPat)
                 ),
            keyword TkLeftBracket ~> inBracket (ListPat <$> sepByComma lPat),
            LitPat <$> lit
          ]

fixSig :: Parser (LSig HsixPs)
fixSig =
  XRec
    <$> withSemi
      ( do
          keyword TkInfix
          ops <- sepByComma1 varOpP
          grp <- upperNameIdP
          return (FixSig FixitySig {fsId = ops, fsPrecGroup = grp})
      )

lSigType :: Parser (LSigType HsixPs)
lSigType =
  XRec <$> do
    bndrs <-
      ( do
          keyword TkForall
          bndrs <- many lInvisTyVarBndr
          dot
          return OuterExplicit {oBndrs = bndrs}
        )
        <|> return OuterImplicit
    body <- ctxtType
    return Sig {sigBndrs = bndrs, sigBody = body}

classOpSig :: Parser (LSig HsixPs)
classOpSig =
  XRec
    <$> withSemi
      ( do
          ids <- sepByComma1 varIdP
          colon
          ClassOpSig ids <$> lSigType
      )

typeSig :: Parser (LSig HsixPs)
typeSig =
  XRec
    <$> withSemi
      ( do
          ids <- sepByComma1 varIdP
          colon
          (\t -> TypeSig ids WC {wcBody = t}) <$> lSigType
      )

stmt :: Parser a -> Parser (StmtLR HsixPs HsixPs a)
stmt body =
  try
    ( do
        p <- lPat
        keyword TkLeftArrow
        BindStmt p <$> body
    )
    <|> (BodyStmt <$> body)

exprStmt = XRec <$> stmt lExpr

lgrhs :: Parser () -> Parser a -> Parser (LGRHS HsixPs a)
lgrhs sym body =
  do
    pat <-
      (keyword TkBar >> sepByComma (XRec <$> stmt lExpr))
        <|> return []
    sym
    XRec . GRHS pat <$> body

grhss :: Parser (LSig HsixPs) -> Parser () -> Parser a -> Parser (GRHSs HsixPs a)
grhss sig sym body =
  do
    rhss <- many (lgrhs sym body)
    lb <- localBinds sig
    return GRHSs {grhssGRHSs = rhss, grhssLocalBinds = lb}

bind :: Parser (LSig HsixPs) -> Parser (LBindLR HsixPs HsixPs)
bind sig =
  XRec
    <$> ( try
            ( withSemi $ do
                pat <- lPat
                when (isVarPatRec $ unXRec pat) (failure Nothing Set.empty)
                rhs <- grhss typeSig equal lExpr
                return PatBind {patLhs = pat, patRhs = rhs}
            )
            <|> ( do
                    fst <- prefix varIdP
                    let name = (mcFun . mCtxt . unXRec) fst
                    rst <-
                      many $
                        prefix
                          ( try $ do
                              n <- varIdP
                              if n /= name
                                then failure Nothing Set.empty
                                else return n
                          )
                    return
                      FunBind
                        { funId = name,
                          funMatches = MG {mgAlts = XRec (fst : rst), origin = FromSource}
                        }
                )
        )
  where
    prefix pred =
      XRec
        <$> withSemi
          ( do
              n <- pred
              pats <- many lPat
              grh <- grhss sig equal lExpr
              return
                Match
                  { mCtxt =
                      FunRHS {mcFun = n, mcFixity = Prefix},
                    mPats = pats,
                    mGRHSS = grh
                  }
          )

valBinds :: Parser (LSig HsixPs) -> Parser (ValBindsLR HsixPs HsixPs)
valBinds sig =
  foldr
    (\(ValBinds b1 s1) (ValBinds b2 s2) -> ValBinds (b1 ++ b2) (s1 ++ s2))
    (ValBinds [] [])
    <$> many
      ( choice
          [ keyword TkInfix ~> (ValBinds [] . singleton <$> fixSig),
            ValBinds [] . singleton <$> try sig,
            (\b -> ValBinds [b] []) <$> bind sig
          ]
      )

localBinds :: Parser (LSig HsixPs) -> Parser (LocalBinds HsixPs)
localBinds sig =
  (keyword TkWhere >> (LocalValBinds <$> inBrace (valBinds sig)))
    <|> return EmptyLocalBinds

bindAndSig :: Parser (LSig HsixPs) -> Parser (LBinds HsixPs, [LSig HsixPs])
bindAndSig sig =
  foldr (\(bd, sig) (bds, sigs) -> (bd ++ bds, sig ++ sigs)) ([], [])
    <$> many
      ( ((\s -> ([], [s])) <$> try sig)
          <|> ((\v -> ([v], [])) <$> bind typeSig)
      )

lExpr :: Parser (LExpr HsixPs)
lExpr =
  do
    exp <- inExp
    ty <- optional (colon >> lSigType)
    return
      ( case ty of
          Nothing -> exp
          Just t -> XRec (ExprWithTySig exp (WC t))
      )

leftTree :: Parser a -> (a -> Parser a) -> Parser a
leftTree init f = init >>= iter
  where
    iter prev = (f prev >>= iter) <|> return prev

inExp :: Parser (LExpr HsixPs)
inExp =
  leftTree
    funExp
    ( \prev -> do
        op <- varOpP
        XRec . OpApp prev (XRec $ E.Var op) <$> funExp
    )

lDotFieldOcc = XRec . DotFieldOcc . XRec <$> lowerName

funExp :: Parser (LExpr HsixPs)
funExp =
  leftTree
    argExp
    ( \prev ->
        ( do
            keyword TkPrefixAt
            typ <- argType
            return (XRec $ AppType prev WC {wcBody = typ})
        )
          <|> (XRec . App prev <$> argExp)
    )

argExp :: Parser (LExpr HsixPs)
argExp =
  choice
    [ keyword TkPrefixMinus >> (XRec . NegApp <$> argExp),
      keyword TkBackslash
        >> ( do
               pats <- many lPat
               keyword TkArrow
               e <- lExpr
               return $
                 XRec
                   ( Lam
                       MG
                         { mgAlts =
                             XRec
                               [ XRec
                                   ( Match
                                       { mCtxt = LambdaExpr,
                                         mPats = pats,
                                         mGRHSS =
                                           GRHSs
                                             { grhssGRHSs = [XRec (GRHS [] e)],
                                               grhssLocalBinds = EmptyLocalBinds
                                             }
                                       }
                                   )
                               ],
                           origin = FromSource
                         }
                   )
           ),
      keyword TkLet >> XRec <$> do
        b <-
          LocalValBinds <$> inBrace (valBinds (fixSig <|> typeSig))
        keyword TkIn
        Let b <$> lExpr,
      keyword TkIf
        >> inBrace
          ( XRec . MultiIf <$> many (withSemi (lgrhs (keyword TkArrow) lExpr))
          )
          <|> ( do
                  cond <- lExpr
                  keyword TkThen
                  suc <- lExpr
                  keyword TkElse
                  XRec . If cond suc <$> lExpr
              ),
      keyword TkCase >> do
        exp <- lExpr
        optKeyword TkOf
        alts <- inBrace $ do
          mat <- many . withSemi $ do
            pat <- lPat
            rhs <- grhss typeSig (keyword TkArrow) lExpr
            return (XRec Match {mCtxt = CaseAlt, mPats = [pat], mGRHSS = rhs})
          return MG {mgAlts = XRec mat, origin = FromSource}
        return (XRec (Case exp alts)),
      keyword TkDo >> (XRec . Do DoExpr . XRec <$> many (withSemi exprStmt)),
      argExp1
    ]

recordCon con = do
  optKeyword TkWith
  flds <- recFields (XRec $ E.Var punRight) lExpr
  return (XRec (RecordCon {rconCon = con, rconFlds = flds}))

argExp1 :: Parser (LExpr HsixPs)
argExp1 =
  leftTree
    ( ( do
          con <- try conIdP
          ((keyword TkWith <|> keyword TkLeftBrace) ~> recordCon con)
            <|> return (XRec (E.Var con))
      )
        <|> argExp2
    )
    ( \prev ->
        ( keyword TkTightInfixProj
            >> (XRec . GetField prev <$> lDotFieldOcc)
        )
          <|> ( (keyword TkWith <|> keyword TkLeftBrace)
                  ~> do
                    optKeyword TkWith
                    flds <-
                      inBrace . many . withSemi $
                        ( XRec
                            <$> fieldBind
                              (XRec . FieldLabelStrings <$> sepBy1 lDotFieldOcc (keyword TkTightInfixProj))
                              (XRec $ E.Var punRight)
                              lExpr
                        )
                    return (XRec (RecordUpd {rupdExpr = prev, rupdFlds = flds}))
              )
    )

argExp2 :: Parser (LExpr HsixPs)
argExp2 =
  choice
    [ XRec . E.Var <$> try (varIdP <|> conIdP),
      XRec . Lit <$> lit,
      XRec
        <$> inParen
          ( ( keyword TkPrefixProj
                >> Projection . fmap (XRec . DotFieldOcc . XRec) <$> sepBy1 lowerName (keyword TkTightInfixProj)
            )
              <|> ( ( \case
                        [] -> E.Var (XRec . Exact $ mkBuiltInName "()" ["Data", "Unit"])
                        [x] -> Par x
                        xs -> ExplicitTuple (Present <$> xs)
                    )
                      <$> sepByComma tupleExp
                  )
          ),
      inBracket list
    ]

list :: Parser (LExpr HsixPs)
list = XRec . ExplicitList <$> sepByComma lExpr

tupleExp :: Parser (LExpr HsixPs)
tupleExp = lExpr

instDecl :: Parser (InstDecl HsixPs)
instDecl =
  withSemi $ do
    ty <- lSigType
    optWhere
    (bds, sigs) <- inBrace $ bindAndSig typeSig
    return
      ClsInstD
        { cidInst = ClsInstDecl {cidPolyTy = ty, cidBinds = bds, cidSigs = sigs}
        }

topDecl :: Parser (Decl HsixPs)
topDecl =
  choice
    [ keyword TkInstance >> (InstD <$> instDecl),
      try $ do
        attr <- attribute
        vis <- visibility
        choice
          [ keyword TkOpen ~> (OpenD vis <$> openDecl attr),
            keyword TkModule ~> (ModD vis <$> moduleDecl attr),
            keyword TkPrecedence ~> (PrecGrpD vis <$> precedenceGroupDecl),
            keyword TkBlock
              >> ( BlockD attr vis
                     <$> withSemi (optWhere >> inBrace (many topDecl))
                 ),
            TyClD vis <$> tyClDecl attr
          ],
      SigD . unXRec <$> try (fixSig <|> typeSig),
      ValD . unXRec <$> bind typeSig
    ]

initState :: Parser (ModBody HsixPs)
initState = moduleBody <* eof

run :: Parser a -> String -> [Token] -> (Either (ParseErrorBundle [Token] Void) a, ParserState)
run parser name tok =
  runState
    (runParserT parser name tok)
    ParserState {subMod = []}

parse :: Maybe String -> [Token] -> FileMod HsixPs
parse name tok =
  case run initState (fromMaybe "<unknown>" name) tok of
    (Left e, _) -> error ("parse: error\n" ++ show e)
    (Right v, stat) ->
      FileMod
        { fmBody = v,
          fmSubmodule = subMod stat
        }