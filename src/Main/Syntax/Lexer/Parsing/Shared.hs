module Main.Syntax.Lexer.Parsing.Shared (getBodyExprSpaces, getBodyExprRange, getParamSigExprRange, getNameSigExprRange, getBodyExprsRange, getBodyBlockExprsRange) where

import Data.Function (($))
import Data.List (head, last)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Text (Text)
import Main.Syntax.Lexer.LexerModel
  ( BodyExpr (AlphaNumBodyExpr, HigherPriorityExpr, NumberBodyExpr, SpecBodyExpr, TextBodyExpr, UnderscoreBodyExpr),
    ExprsBlock (ExprsBlock, ebExprs, ebSemicolon),
    NameSigExpr (AlphaNumNameSigExpr, SpecNameSigExpr),
    Nested (InBracketsNested, IndentedNested, ibExprs),
    ParamSigExpr (AlphaNumParamSigExpr, HpParamSigExpr, NumberParamSigExpr, TextParamSigExpr),
  )
import Shared.Location.Data (OcRanged (oSpaces, ocRange), Range (Range, from, to), Ranged (Ranged, rSpaces, range))

getBodyExprSpaces :: BodyExpr -> Text
getBodyExprSpaces (AlphaNumBodyExpr i) = rSpaces i
getBodyExprSpaces (SpecBodyExpr i) = rSpaces i
getBodyExprSpaces (UnderscoreBodyExpr i) = rSpaces i
getBodyExprSpaces (NumberBodyExpr i) = rSpaces i
getBodyExprSpaces (TextBodyExpr i) = rSpaces i
getBodyExprSpaces (HigherPriorityExpr (IndentedNested expressions)) = getBodyExprSpaces $ head $ ebExprs $ head expressions
getBodyExprSpaces (HigherPriorityExpr (InBracketsNested {ibExprs = e})) = oSpaces e

getBodyExprRange :: BodyExpr -> Range
getBodyExprRange (AlphaNumBodyExpr i) = range i
getBodyExprRange (SpecBodyExpr i) = range i
getBodyExprRange (UnderscoreBodyExpr i) = range i
getBodyExprRange (NumberBodyExpr i) = range i
getBodyExprRange (TextBodyExpr i) = range i
getBodyExprRange (HigherPriorityExpr (IndentedNested expressions)) =
  Range
    { from = from $ getBodyBlockExprRange $ head expressions,
      to = to $ getBodyBlockExprRange $ last expressions
    }
getBodyExprRange (HigherPriorityExpr (InBracketsNested {ibExprs = e})) = ocRange e

getBodyExprsRange :: [BodyExpr] -> Range
getBodyExprsRange exprs = Range {from = from $ getBodyExprRange $ head exprs, to = to $ getBodyExprRange $ last exprs}

getBodyBlockExprRange :: ExprsBlock -> Range
getBodyBlockExprRange (ExprsBlock {ebExprs = exprs, ebSemicolon = Nothing}) = Range {from = from $ getBodyExprRange $ head exprs, to = to $ getBodyExprRange $ last exprs}
getBodyBlockExprRange (ExprsBlock {ebExprs = [], ebSemicolon = Just (Ranged {range = r})}) =
  Range {from = from r, to = to r}
getBodyBlockExprRange (ExprsBlock {ebExprs = exprs, ebSemicolon = Just (Ranged {range = r})}) =
  Range {from = from r, to = to $ getBodyExprRange $ last exprs}

getBodyBlockExprsRange :: [ExprsBlock] -> Range
getBodyBlockExprsRange exprs = Range {from = from $ getBodyBlockExprRange $ head exprs, to = to $ getBodyBlockExprRange $ last exprs}

getParamSigExprRange :: ParamSigExpr -> Range
getParamSigExprRange (AlphaNumParamSigExpr i) = range i
getParamSigExprRange (NumberParamSigExpr i) = range i
getParamSigExprRange (TextParamSigExpr i) = range i
getParamSigExprRange (HpParamSigExpr e) = ocRange e

getNameSigExprRange :: NameSigExpr -> Range
getNameSigExprRange (AlphaNumNameSigExpr i) = range i
getNameSigExprRange (SpecNameSigExpr i) = range i
