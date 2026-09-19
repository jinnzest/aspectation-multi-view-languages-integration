module Main.Syntax.Lexer.Parsing.Shared (getBodyExprSpaces, getBodyExprRange, getParamSigExprRange, getNameSigExprRange, getBodyExprsRange, getBodyBlockExprsRange) where

import Data.Function (($))
import Data.List.NonEmpty as NE (NonEmpty ((:|)), head, last)
import Data.Text (Text)
import Main.Syntax.Lexer.LexerModel
  ( BodyExpr (AlphaNumBodyExpr, HigherPriorityExpr, NumberBodyExpr, SpecBodyExpr, TextBodyExpr, UnderscoreBodyExpr),
    ExprsBlock (ExprsBlock, ebExprs),
    NameSigExpr (AlphaNumNameSigExpr, SpecNameSigExpr),
    Nested (EmptyNested, InBracketsNested, IndentedNested, ibExprs),
    ParamSigExpr (AlphaNumParamSigExpr, HpParamSigExpr, NumberParamSigExpr, TextParamSigExpr),
  )
import Shared.Location.Data (OcRanged (oSpaces, ocRange), Range (Range, from, to), Ranged (rSpaces, range))

getBodyExprSpaces :: BodyExpr -> Text
getBodyExprSpaces (AlphaNumBodyExpr i) = rSpaces i
getBodyExprSpaces (SpecBodyExpr i) = rSpaces i
getBodyExprSpaces (UnderscoreBodyExpr i) = rSpaces i
getBodyExprSpaces (NumberBodyExpr i) = rSpaces i
getBodyExprSpaces (TextBodyExpr i) = rSpaces i
getBodyExprSpaces (HigherPriorityExpr (IndentedNested expressions)) = getBodyExprSpaces $ NE.head $ ebExprs $ NE.head expressions
getBodyExprSpaces (HigherPriorityExpr (InBracketsNested {ibExprs = e})) = oSpaces e
getBodyExprSpaces (HigherPriorityExpr (EmptyNested _)) = ""

getBodyExprRange :: BodyExpr -> Range
getBodyExprRange (AlphaNumBodyExpr i) = range i
getBodyExprRange (SpecBodyExpr i) = range i
getBodyExprRange (UnderscoreBodyExpr i) = range i
getBodyExprRange (NumberBodyExpr i) = range i
getBodyExprRange (TextBodyExpr i) = range i
getBodyExprRange (HigherPriorityExpr (IndentedNested expressions)) =
  Range
    { from = from $ getBodyBlockExprRange $ NE.head expressions,
      to = to $ getBodyBlockExprRange $ NE.last expressions
    }
getBodyExprRange (HigherPriorityExpr (InBracketsNested {ibExprs = e})) = ocRange e
getBodyExprRange (HigherPriorityExpr (EmptyNested ranged)) = range ranged

getBodyExprsRange :: NonEmpty BodyExpr -> Range
getBodyExprsRange exprs@((HigherPriorityExpr (EmptyNested _)) :| nextExpr : _) =
  Range {from = from $ getBodyExprRange nextExpr, to = to $ getBodyExprRange $ NE.last exprs}
getBodyExprsRange exprs =
  Range {from = from $ getBodyExprRange $ NE.head exprs, to = to $ getBodyExprRange $ NE.last exprs}

getBodyBlockExprRange :: ExprsBlock -> Range
getBodyBlockExprRange (ExprsBlock {ebExprs = exprs}) = Range {from = from $ getBodyExprRange $ NE.head exprs, to = to $ getBodyExprRange $ NE.last exprs}

getBodyBlockExprsRange :: NonEmpty ExprsBlock -> Range
getBodyBlockExprsRange exprs = Range {from = from $ getBodyBlockExprRange $ NE.head exprs, to = to $ getBodyBlockExprRange $ NE.last exprs}

getParamSigExprRange :: ParamSigExpr -> Range
getParamSigExprRange (AlphaNumParamSigExpr i) = range i
getParamSigExprRange (NumberParamSigExpr i) = range i
getParamSigExprRange (TextParamSigExpr i) = range i
getParamSigExprRange (HpParamSigExpr e) = ocRange e

getNameSigExprRange :: NameSigExpr -> Range
getNameSigExprRange (AlphaNumNameSigExpr i) = range i
getNameSigExprRange (SpecNameSigExpr i) = range i
