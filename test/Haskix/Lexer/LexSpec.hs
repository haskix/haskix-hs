{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Haskix.Lexer.LexSpec (spec) where

import Control.Exception (evaluate)
import Data.String.Interpolate
import Haskix.Haskix.Lit
import Haskix.Haskix.Token
import Haskix.Lexer.Lex
import Test.Hspec
import Test.Hspec.Megaparsec
import Prelude hiding (lex)

shouldLexToTk :: String -> [TokenKind] -> Expectation
shouldLexToTk s ts = (woVal <$> lex (Just "<test>") s) `shouldBe` (PtkToken <$> ts)

spec =
  describe "preLex" $ do
    describe "number" $ do
      context "integer with base" $ do
        it "hex" $ do
          "0x12b" `shouldLexToTk` [TkInteger IL {ilVal = 0x12b, ilText = "0x12b", ilNeg = False}]

      context "rational" $ do
        let lexTo str val =
              str `shouldLexToTk` [TkRational FL {flVal = val, flText = str, flNeg = False}]

        it "without exp" $ do
          "0.12" `lexTo` 0.12
        it "with exp" $ do
          "0.12e3" `lexTo` 0.12e3
          "1e3" `lexTo` 1e3

    describe "char" $ do
      let lexTo str char = str `shouldLexToTk` [TkChar char]
      it "symbol" $ do
        [i|'+'|] `lexTo` '+'
        [i|'-'|] `lexTo` '-'
        [i|'\\"'|] `lexTo` '\"'

    describe "symbol" $ do
      it "special" $ do
        "->" `shouldLexToTk` [TkArrow]
        ".." `shouldLexToTk` [TkDotDot]

      context "fixity" $ do
        it "prefix" $ do
          "(." `shouldLexToTk` [TkLeftParen, TkPrefixProj]
