module Main.Syntax.Lexer.Writing.Ranged
  ( writeFuncs,
  )
where

import Data.Function (const, flip, ($), (.))
import Data.Int (Int)
import Data.List as L (length, map, zipWith)
import Data.List.NonEmpty as NE (NonEmpty, fromList, head, last, map, toList)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Monoid (Monoid (mconcat))
import Data.Text as T (Text, null)
import GHC.Num (Num ((+)))
import Main.Syntax.Lexer.LexerModel (BodyExpr (AlphaNumBodyExpr, HigherPriorityExpr, NumberBodyExpr, SpecBodyExpr, TextBodyExpr, UnderscoreBodyExpr), ExprsBlock (ExprsBlock, ebExprs, ebSemicolon), Func (Func, fBody, fSig), FuncBody (FuncBody), FuncSig (FuncSig, fsName, fsParams), FuncSigParams (InfixSigParams, PrefixSigParams), HoParamParams (HoInfixParamParams, HoPrefixParamParams), HpParamSigExpr (FoParamSigExpr, HoParamSigExpr, hpseName, hpseParams), NameSigExpr (AlphaNumNameSigExpr, SpecNameSigExpr), Nested (EmptyNested, InBracketsNested, IndentedNested, ibExprs), Number (Number, dec, exp, mant, sign), ParamSigExpr (AlphaNumParamSigExpr, HpParamSigExpr, NumberParamSigExpr, TextParamSigExpr))
import Main.Syntax.Lexer.Parsing.Shared (getBodyExprRange, getBodyExprsRange, getNameSigExprRange, getParamSigExprRange)
import Shared.Location.Data (OcRanged (ocItem, ocRange), Range (Range, from, to), Ranged (Ranged, rItem, range))
import Shared.Text.Utils (indent, nL)
import Text.Shakespeare.Text (ToText (toText), st)
import Text.Show (Show (show))

nlIndent :: Int -> Text
nlIndent ind = [st|#{nL}#{indent ind}|]

writeRanged :: Text -> (Int -> a -> Text) -> Ranged a -> Int -> Text
writeRanged label builder ranged ind =
  let nextInd = ind + 1
      built = builder nextInd $ rItem ranged
   in [st|#{nlIndent ind}#{range ranged} #{label}#{built}|]

writeOcRanged :: Text -> (Int -> a -> Text) -> OcRanged a -> Int -> Text
writeOcRanged label builder ranged ind =
  let nextInd = ind + 1
      built = builder nextInd $ ocItem ranged
   in [st|#{nlIndent ind}#{ocRange ranged} #{label}#{built}|]

writeIndented :: Int -> Text -> Text
writeIndented ind identifier = [st|#{nL}#{indent ind}#{identifier}|]

writeExprsBlock :: Int -> ExprsBlock -> Text
writeExprsBlock ind eb@ExprsBlock {ebExprs = exprs, ebSemicolon = semicolon} =
  let nextInd = ind + 1
      exprsBlockRange = toText $ getExprsBlockRange eb
      simicolonText = case semicolon of
        Nothing -> ""
        Just (Ranged {range = r}) -> [st|#{nlIndent nextInd}#{r} semicolon|]
      exprsBlockText = case semicolon of
        Nothing -> mconcat $ L.map (`writeBodyExpr` nextInd) $ NE.toList exprs
        Just _ ->
          let nextNextInd = nextInd + 1
              exprsText = mconcat $ L.map (`writeBodyExpr` nextNextInd) $ NE.toList exprs
              exprsRange = getBodyExprsRange exprs
           in if T.null exprsText
                then [st|#{nlIndent nextInd}#{exprsRange} empty nested|]
                else [st|#{nlIndent nextInd}#{exprsRange} expressions#{exprsText}|]
   in [st|#{nlIndent ind}#{exprsBlockRange} expressions block#{simicolonText}#{exprsBlockText}|]

getExprsBlockRange :: ExprsBlock -> Range
getExprsBlockRange (ExprsBlock {ebExprs = exprs}) = Range {from = from $ getBodyExprRange $ NE.head exprs, to = to $ getBodyExprRange $ NE.last exprs}

-- getExprsBlockRange (ExprsBlock {ebExprs = exprs, ebSemicolon = Just Ranged {range = r}}) =
--   Range {from = from r, to = to $ getBodyExprRange $ NE.last exprs}

writeNameExpr :: NameSigExpr -> Int -> Text
writeNameExpr (AlphaNumNameSigExpr ranged) = writeRanged "alphnumeric identifier" writeIndented ranged
writeNameExpr (SpecNameSigExpr ranged) = writeRanged "special identifier" writeIndented ranged

writeHpParamSigExpr :: HpParamSigExpr -> Int -> Text
writeHpParamSigExpr HoParamSigExpr {hpseName = name, hpseParams = HoInfixParamParams _ _} ind =
  let nextInd = ind + 1
   in [st|#{nlIndent ind}higher order infix parameter#{writeNameExpr name nextInd}|]
writeHpParamSigExpr HoParamSigExpr {hpseName = name, hpseParams = HoPrefixParamParams params} ind =
  let nextInd = ind + 1
      nextNextInd = nextInd + 1
      nextNextNextInd = nextNextInd + 1
   in [st|#{nlIndent ind}higher order prefix parameter#{writeNameExpr name nextNextInd}#{nlIndent nextNextInd}params count#{nlIndent nextNextNextInd}#{length params}|]
writeHpParamSigExpr (FoParamSigExpr name) ind =
  let nextInd = ind + 1
   in [st|#{nlIndent ind}first order parameter#{writeParamSigExpr name  nextInd}|]

writeParamSigExpr :: ParamSigExpr -> Int -> Text
writeParamSigExpr (AlphaNumParamSigExpr ranged) = writeRanged "alphnumeric identifier" writeIndented ranged
writeParamSigExpr (NumberParamSigExpr ranged) =
  writeRanged
    "number"
    ( \ind Number {mant, dec, exp} ->
        let nextInd = ind + 1
            decimalText = case dec of
              Nothing -> ""
              Just d -> [st|#{nlIndent ind}decimal#{nlIndent nextInd}#{show d}|]
            exponentText = case exp of
              Nothing -> ""
              Just e -> [st|#{nlIndent ind}exponent#{nlIndent nextInd}#{show e}|]
         in [st|#{nlIndent ind}integer#{nlIndent nextInd}#{show mant}#{decimalText}#{exponentText}|]
    )
    ranged
writeParamSigExpr (TextParamSigExpr ranged) = writeRanged "text" writeIndented ranged
writeParamSigExpr (HpParamSigExpr pp) =
  writeOcRanged "higher priority parameter" (flip writeHpParamSigExpr) pp

writeBodyExpr :: BodyExpr -> Int -> Text
writeBodyExpr (AlphaNumBodyExpr ranged) = writeRanged "alphnumeric identifier" writeIndented ranged
writeBodyExpr (SpecBodyExpr ranged) = writeRanged "special identifier" writeIndented ranged
writeBodyExpr (NumberBodyExpr ranged) =
  writeRanged
    "number"
    ( \ind Number {mant, dec, exp, sign} ->
        let nextInd = ind + 1
            decText = case dec of
              Nothing -> ""
              Just d -> [st|#{nlIndent ind}decimal#{nlIndent nextInd}#{show d}|]
            expText = case exp of
              Nothing -> ""
              Just e -> [st|#{nlIndent ind}exponent#{nlIndent nextInd}#{show e}|]
            signed = if sign then [st|#{nlIndent ind}signed|] else ""
         in [st|#{nlIndent ind}integer#{nlIndent nextInd}#{show mant}#{decText}#{expText}#{signed}|]
    )
    ranged
writeBodyExpr (TextBodyExpr ranged) = writeRanged "text" writeIndented ranged
writeBodyExpr (HigherPriorityExpr (IndentedNested expressions)) = \ind ->
  let expressionsText = mconcat $ L.map (writeExprsBlock (ind + 1)) $ NE.toList expressions
      exprRange = Range {from = from $ getExprsBlockRange $ NE.head expressions, to = to $ getExprsBlockRange $ NE.last expressions}
   in [st|#{nlIndent ind}#{exprRange} nested expressions#{expressionsText}|]
writeBodyExpr (HigherPriorityExpr InBracketsNested {ibExprs}) =
  writeOcRanged "higher priority expressions" (\nextInd expr -> mconcat $ NE.toList $ NE.map (writeExprsBlock nextInd) expr) ibExprs
writeBodyExpr (HigherPriorityExpr (EmptyNested _)) = const ""
writeBodyExpr (UnderscoreBodyExpr ranged) =
  writeRanged "underscore" (\_ _ -> "") ranged

writeBody :: Ranged (NonEmpty ExprsBlock) -> Int -> Text
writeBody =
  writeRanged
    "function body"
    (\nextInd expression -> let expressionTexts = NE.map (writeExprsBlock nextInd) expression in mconcat $ NE.toList expressionTexts)

writeInfixParams :: ParamSigExpr -> ParamSigExpr -> Int -> Text
writeInfixParams leftParam rightParam ind =
  let nextInd = ind + 1
      nextNextInd = nextInd + 1
      argsText = [st|#{nlIndent nextInd}#{getParamSigExprRange leftParam} left parameter #{writeParamSigExpr leftParam nextNextInd}#{nlIndent nextInd}#{getParamSigExprRange rightParam} right parameter #{writeParamSigExpr rightParam nextNextInd}|]
      f = from $ getParamSigExprRange leftParam
      t = to $ getParamSigExprRange rightParam
      argsRange = [st|#{f} - #{t}|]
   in [st|#{nlIndent ind}#{argsRange} parameters#{argsText}|]

writePrefixParams :: [ParamSigExpr] -> Int -> Text
writePrefixParams [] _ = ""
writePrefixParams args ind = writePrefixParamsNE ind $ NE.fromList args

writePrefixParamsNE :: Int -> NonEmpty ParamSigExpr -> Text
writePrefixParamsNE ind args =
  let nextInd = ind + 1
      nextNextInd = nextInd + 1
      argsText =
        mconcat
          $ zipWith
            ( \i a ->
                let argRange = getParamSigExprRange a
                 in [st|#{nlIndent nextInd}#{argRange} parameter #{i}#{writeParamSigExpr a nextNextInd}|]
            )
            [1 :: Int ..]
          $ NE.toList args
      f = from $ getParamSigExprRange $ NE.head args
      t = to $ getParamSigExprRange $ NE.last args
      argsRange = [st|#{f} - #{t}|]
   in [st|#{nlIndent ind}#{argsRange} parameters#{argsText}|]

writeFuncSig :: FuncSig -> Int -> Text
writeFuncSig FuncSig {fsName, fsParams = PrefixSigParams params} ind =
  let nextInd = ind + 1
      nextNextInd = nextInd + 1
      namesRange = getNameSigExprRange fsName
      namesText = writeIndented nextInd [st|#{namesRange} name#{writeNameExpr fsName nextNextInd}|]
      argsText = writePrefixParams params nextInd
      itemsRange = getNameSigExprRange fsName
   in [st|#{nlIndent ind}#{itemsRange} prefix function signature#{namesText}#{argsText}|]
writeFuncSig FuncSig {fsName, fsParams = InfixSigParams leftParam rightParam} ind =
  let nextInd = ind + 1
      nextNextInd = nextInd + 1
      namesRange = getNameSigExprRange fsName
      namesText = writeIndented nextInd [st|#{namesRange} name#{writeNameExpr fsName nextNextInd}|]
      argsText = writeInfixParams leftParam rightParam nextInd
      itemsRange = getNameSigExprRange fsName
   in [st|#{nlIndent ind}#{itemsRange} infix function signature#{namesText}#{argsText}|]

writeFunc :: Ranged Func -> Int -> Text
writeFunc ranged =
  writeRanged
    ( case rItem ranged of
        Func {} -> "function"
    )
    ( \nextInd c -> case c of
        Func {fSig = signature, fBody = FuncBody body} ->
          let signatureText = writeFuncSig signature nextInd
              bodyText = writeBody body nextInd
           in [st|#{signatureText}#{bodyText}|]
    )
    ranged

writeFuncs :: [Ranged Func] -> Text
writeFuncs = mconcat . L.map (`writeFunc` 0)
