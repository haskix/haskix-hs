{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Haskix.Lexer.Lex (lex, initState, run) where

import Control.Monad.State
import Data.Char
import Data.List (takeWhile)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Void (Void)
import Haskix.Haskix.Lit
import Haskix.Haskix.Token
import Numeric
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Read.Lex (isSymbolChar)
import Prelude hiding (lex, takeWhile)

data LexState = LexState
  { lsAfterClose :: Bool,
    lsIndent :: Int
  }

type Parser = ParsecT Void String (State LexState)

(~>) :: Parser a -> Parser b -> Parser b
l ~> b = lookAhead l >> b

trailingSpace :: Bool -> Parser ()
trailingSpace close =
  (hspace1 >> modify (\s -> s {lsAfterClose = False}))
    <|> modify (\s -> s {lsAfterClose = close})

getPos :: Parser Position
getPos = do
  off <- getOffset
  pos <- getSourcePos
  return Position {posOffset = off, posSource = pos}

withPos :: Bool -> Parser TokenKind -> Parser PreToken
withPos close tk = do
  pos1 <- getPos
  tok <- tk
  pos2 <- getPos
  trailingSpace close
  return
    ( WithOffset
        { woOffset = Just (pos1, pos2),
          woVal = PtkToken tok
        }
    )

constToken :: String -> Bool -> TokenKind -> Parser PreToken
constToken str close tk =
  withPos close (void (string str) >> return tk)

name :: Parser PreToken
name =
  withPos
    True
    ( ( \case
          "block" -> TkBlock
          "case" -> TkCase
          "class" -> TkClass
          "data" -> TkData
          "do" -> TkDo
          "deriving" -> TkDeriving
          "else" -> TkElse
          "flake" -> TkFlake
          "forall" -> TkForall
          "if" -> TkIf
          "in" -> TkIn
          "infix" -> TkInfix
          "instance" -> TkInstance
          "let" -> TkLet
          "module" -> TkModule
          "newtype" -> TkNewtype
          "of" -> TkOf
          "open" -> TkOpen
          "precedence" -> TkPrecedence
          "private" -> TkPrivate
          "record" -> TkRecord
          "self" -> TkSelf
          "super" -> TkSuper
          "type" -> TkType
          "then" -> TkThen
          "using" -> TkUsing
          "where" -> TkWhere
          "with" -> TkWith
          s@(p : _) | isUpper p -> TkUpperName s
          v -> TkLowerName v
      )
        <$> takeWhileP
          (Just "name or keyword")
          ( \x -> isAlphaNum x || x == '\'' || x == '_'
          )
    )

intWithBase :: Parser PreToken
intWithBase =
  withPos True $ do
    void (char '0')

    let int :: Char -> (String -> Integer) -> (Char -> Bool) -> Parser TokenKind
        int prefix reader digit =
          do
            pref <- char' prefix
            str <- takeWhile1P (Just "digits") digit
            return
              ( TkInteger
                  IL
                    { ilVal = reader str,
                      ilText = '0' : pref : str,
                      ilNeg = False
                    }
              )

    choice
      [ int 'b' readBin (\x -> x == '0' || x == '1'),
        int 'o' (fst . head . readOct) isOctDigit,
        int 'd' read isDigit,
        int 'x' (fst . head . readHex) isHexDigit
      ]
  where
    readBin :: String -> Integer
    readBin = foldl (\acc v -> acc * 2 + (if v == '0' then 0 else 1)) 0

number :: Parser PreToken
number =
  withPos True $ do
    (dot, base) <- getBase

    ( do
        e <- char' 'e'
        exp <- takeWhile1P (Just "Rational exp") isDigit
        return
          ( TkRational
              ( let text = base ++ (e : exp)
                 in FL
                      { flVal = (fst . head . readFloat) text,
                        flText = text,
                        flNeg = False
                      }
              )
          )
      )
      <|> return
        ( if dot
            then
              TkRational
                FL
                  { flVal = (fst . head . readFloat) base,
                    flText = base,
                    flNeg = False
                  }
            else
              TkInteger
                IL
                  { ilVal = read base,
                    ilText = base,
                    ilNeg = False
                  }
        )
  where
    getBase :: Parser (Bool, String)
    getBase = do
      base1 <- takeWhile1P (Just "int digit or rational base") isDigit
      ( do
          d <- char '.'
          base2 <- takeWhile1P (Just "rational value after dot") isDigit
          return (True, base1 ++ (d : base2))
        )
        <|> return (False, base1)

charLit :: Parser PreToken
charLit =
  withPos True $ do
    void (char '\'')
    val <- L.charLiteral
    void (char '\'')
    return (TkChar val)

stringLit :: Parser PreToken
stringLit =
  withPos
    True
    (TkString <$> (char '"' >> manyTill L.charLiteral (char '"')))

maybeLineComment :: Parser PreToken
maybeLineComment = do
  begin <- getPos
  str1 <- takeWhileP Nothing (== '-')

  ( do
      void (char ' ')
      typ <- takeWhileP (Just "prefix") (/= ' ')
      body <- takeWhileP (Just "body") (/= '\n')
      pos2 <- getPos
      modify (\s -> s {lsAfterClose = False})
      return
        ( WithOffset
            { woOffset = Just (begin, pos2),
              woVal = PtkToken (TkInlineComment {tkIcPrefix = if null typ then Nothing else Just typ, tkIcContent = body})
            }
        )
    )
    <|> ( do
            rest <- takeWhileP (Just "symbol body") isSymbolChar
            pos2 <- getPos
            trailingSpace True
            return
              ( WithOffset
                  { woOffset = Just (begin, pos2),
                    woVal = PtkToken (TkVarSymbol (str1 ++ rest))
                  }
              )
        )

attrib :: Parser PreToken
attrib =
  withPos False $ do
    void (string "{-@")
    space
    (string "MOD" >> return TkGlobalAttr)
      <|> return TkOpenAttr

blockComment :: Parser PreToken
blockComment =
  withPos False $ do
    void (string "{-")
    space
    typ <- takeWhileP (Just "prefix") (/= ' ')
    body <- manyTill anySingle (string "-}")
    return
      ( TkBlockComment
          { tkBcPrefix = if null typ then Nothing else Just typ,
            tkBcContent = body
          }
      )

fixity ::
  (String -> TokenKind) ->
  -- | prefix
  TokenKind ->
  -- | tight infix
  TokenKind ->
  -- | infix
  TokenKind ->
  -- | suffix
  TokenKind ->
  Parser TokenKind
fixity normal prefix tightInfix infixOp suffix =
  do
    afterClose <- gets lsAfterClose
    choice
      [ space1 ~> return (if afterClose then suffix else infixOp),
        token (\c -> if isSymbolChar c then Just () else Nothing) Set.empty
          ~> ( normal <$> takeWhileP (Just "symbol") isSymbolChar
             ),
        return (if afterClose then tightInfix else prefix)
      ]

at :: Parser PreToken
at =
  withPos True $ do
    void (char '@')
    fixity
      (\v -> TkVarSymbol ('@' : v))
      TkPrefixAt
      TkTightInfixAt
      (TkVarSymbol "@")
      (TkVarSymbol "@")

dot :: Parser PreToken
dot =
  withPos True $ do
    void (char '.')
    fixity
      ( \v ->
          case '.' : v of
            ".." -> TkDotDot
            s -> TkVarSymbol s
      )
      TkPrefixProj
      TkTightInfixProj
      (TkVarSymbol ".")
      (TkVarSymbol ".")

bang :: Parser PreToken
bang =
  withPos True $ do
    void (char '!')
    fixity
      (\v -> TkVarSymbol ('!' : v))
      (TkVarSymbol "!")
      (TkVarSymbol "!")
      (TkVarSymbol "!")
      TkSuffixBang

minus :: Parser PreToken
minus =
  withPos True $ do
    void (char '-')
    fixity
      ( \v ->
          case '-' : v of
            "->" -> TkArrow
            v -> TkVarSymbol v
      )
      TkPrefixMinus
      (TkVarSymbol "-")
      (TkVarSymbol "-")
      (TkVarSymbol "-")

symbol :: Parser PreToken
symbol =
  withPos
    True
    ( ( \case
          "=>" -> TkFatArrow
          ":" -> TkColon
          "=" -> TkEqual
          "\\" -> TkBackslash
          "<-" -> TkLeftArrow
          "|" -> TkBar
          "*" -> TkStar
          v@(':' : _) -> TkConstructorSymbol v
          v -> TkVarSymbol v
      )
        <$> takeWhileP (Just "symbol") isSymbolChar
    )

line :: Parser PreToken
line = do
  skipMany eol -- skip this eol and empty lines
  stat <- get
  new <- length <$> takeWhileP Nothing (== ' ')
  put LexState {lsAfterClose = False, lsIndent = new}
  let old = lsIndent stat
  return
    ( WithOffset
        { woOffset = Nothing,
          woVal =
            case compare new old of
              LT -> PtkDedent IndentChange {icFrom = old, icTo = new}
              EQ -> PtkEol new
              GT -> PtkIndent IndentChange {icFrom = old, icTo = new}
        }
    )

start :: Parser PreToken
start =
  choice
    [ token (fromPred isAlpha) Set.empty ~> name,
      choice (string <$> ["0b", "0o", "0d", "0x"]) ~> intWithBase,
      token (fromPred isDigit) Set.empty ~> number,
      char '\'' ~> charLit,
      char '"' ~> stringLit,
      string "--" ~> maybeLineComment,
      string "{-@" ~> attrib,
      string "{-" ~> blockComment,
      string "@-}" ~> constToken "@-}" False TkCloseAttr,
      singleCharToken '{' False TkLeftBrace,
      singleCharToken '}' True TkRightBrace,
      singleCharToken '(' False TkLeftParen,
      singleCharToken ')' True TkRightParen,
      singleCharToken '[' False TkLeftBracket,
      singleCharToken ']' True TkRightBracket,
      singleCharToken ';' False TkSemicolon,
      singleCharToken ',' False TkComma,
      singleCharToken '`' True TkBacktick,
      singleCharToken '_' True TkUnderline,
      char '@' ~> at,
      char '.' ~> dot,
      char '!' ~> bang,
      char '-' ~> minus,
      token (fromPred isSymbolChar) Set.empty ~> symbol,
      char '\n' ~> line
    ]
  where
    fromPred :: (Char -> Bool) -> Char -> Maybe Char
    fromPred f x = if f x then Just x else Nothing

    singleCharToken :: Char -> Bool -> TokenKind -> Parser PreToken
    singleCharToken ch close tok =
      char ch ~> constToken [ch] close tok

initState :: Parser [PreToken]
initState = space >> many start <* eof

run :: Parser a -> String -> String -> Either (ParseErrorBundle String Void) a
run parser file string =
  evalState
    (runParserT parser file string)
    LexState {lsAfterClose = True, lsIndent = 0}

lex ::
  -- | file name
  Maybe String ->
  -- | input string
  String ->
  [PreToken]
lex file input = case run initState (fromMaybe "<unknown>" file) input of
  Left err -> error ("lex: failed\n" ++ errorBundlePretty err)
  Right v -> v