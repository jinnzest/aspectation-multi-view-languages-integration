module Main.Syntax.Lexer.Writing.Source
  ( writeFuncs,
    writeFuncSig,
  )
where

import Control.Category (Category (id))
import Control.Monad.Writer (Monoid (mconcat))
import Data.Eq (Eq ((==)))
import Data.Foldable (Foldable (foldl))
import Data.Function (const, ($))
import Data.List (map, reverse)
import Data.Maybe (Maybe (Just, Nothing), maybe)
import Data.String (String)
import Data.Text as T (Text, pack, unpack)
import Main.Syntax.Lexer.LexerModel
  ( BodyExpr
      ( AlphaNumBodyExpr,
        HigherPriorityExpr,
        NumberBodyExpr,
        SpecBodyExpr,
        TextBodyExpr,
        UnderscoreBodyExpr
      ),
    ExprsBlock (ExprsBlock, ebExprs, ebSemicolon),
    Func (Func, fBody, fSig),
    FuncBody (FuncBody),
    FuncSig (FuncSig, fsName, fsParams),
    FuncSigParams (InfixSigParams, PrefixSigParams),
    HoParamParams (HoInfixParamParams, HoPrefixParamParams),
    HpParamSigExpr (FoParamSigExpr, HoParamSigExpr, hpseName, hpseParams),
    NameSigExpr (AlphaNumNameSigExpr, SpecNameSigExpr),
    Nested (InBracketsNested, IndentedNested, ibExprs, ibKind),
    Number (Number, dec, exp, mant, sign),
    ParamSigExpr (AlphaNumParamSigExpr, HpParamSigExpr, NumberParamSigExpr, TextParamSigExpr),
  )
import Shared.Location.Data
  ( BracketsKind (CurlyBrackets, RoundBrackets, SquareBrackets),
    OcRanged
      ( OcRanged,
        cSpaces,
        oSpaces,
        ocItem
      ),
    Ranged (Ranged, rItem, rSpaces),
  )
import Text.Shakespeare.Text as ST (st)
import Text.Show (Show (show))

writeRangedWith :: (a -> Text) -> Ranged a -> Text
writeRangedWith builder Ranged {rItem, rSpaces} = [st|#{rSpaces}#{builder rItem}|]

writeRanged :: Ranged Text -> Text
writeRanged = writeRangedWith id

writeNumber :: Ranged Number -> Text
writeNumber Ranged {rItem = Number {mant, dec, exp, sign}, rSpaces} =
  let decimalText = maybe "" (\d -> [st|.#{show d}|]) dec
      exponentText = maybe "" (\e -> [st|e#{show e}|]) exp
      signed :: String
      signed = if sign then "-" else ""
   in [st|#{rSpaces}#{signed}#{show mant}#{decimalText}#{exponentText}|]

writeParamSigExpr :: ParamSigExpr -> Text
writeParamSigExpr (AlphaNumParamSigExpr r) = writeRanged r
writeParamSigExpr (NumberParamSigExpr r) = writeNumber r
writeParamSigExpr (TextParamSigExpr Ranged {rItem, rSpaces}) =
  let escapedText = pack $ reverse $ foldl (\acc c -> if c == '"' then c : c : acc else c : acc) "" $ unpack rItem
   in [st|#{rSpaces}"#{escapedText}"|]
writeParamSigExpr (HpParamSigExpr OcRanged {ocItem = FoParamSigExpr expr, oSpaces = oS, cSpaces = cS}) = [st|#{oS}(#{writeParamSigExpr expr}#{cS})|]
writeParamSigExpr (HpParamSigExpr OcRanged {ocItem = HoParamSigExpr {hpseName, hpseParams = HoPrefixParamParams params}, oSpaces = oS, cSpaces = cS}) =
  let paramsText = mconcat $ map (writeRangedWith (const "_")) params
      namesText = writeNameSigExpr hpseName
   in [st|#{oS}(#{namesText}#{paramsText}#{cS})|]
writeParamSigExpr (HpParamSigExpr OcRanged {ocItem = HoParamSigExpr {hpseName, hpseParams = HoInfixParamParams leftParam rightParam}, oSpaces = oS, cSpaces = cS}) =
  [st|#{oS}(#{writeRangedWith (const "_") leftParam}#{writeNameSigExpr hpseName}#{writeRangedWith (const "_") rightParam}#{cS})|]

writeNameSigExpr :: NameSigExpr -> Text
writeNameSigExpr (AlphaNumNameSigExpr Ranged {rItem, rSpaces}) = [st|#{rSpaces}#{rItem}|]
writeNameSigExpr (SpecNameSigExpr Ranged {rItem, rSpaces}) = [st|#{rSpaces}#{rItem}|]

writeFuncSig :: FuncSig -> Text
writeFuncSig FuncSig {fsName, fsParams = PrefixSigParams params} =
  mconcat $ writeNameSigExpr fsName : map writeParamSigExpr params
writeFuncSig FuncSig {fsName, fsParams = InfixSigParams leftParam rightParam} =
  mconcat [writeParamSigExpr leftParam, writeNameSigExpr fsName, writeParamSigExpr rightParam]

writeExprsBlock :: ExprsBlock -> Text
writeExprsBlock ExprsBlock {ebExprs = expressions, ebSemicolon = semicolon} =
  let exprsText = mconcat $ map writeBodyExpr expressions
      semicolonText = case semicolon of
        Nothing -> ""
        Just Ranged {rSpaces} -> [st|#{rSpaces};|]
   in [st|#{semicolonText}#{exprsText}|]

writeBodyExpr :: BodyExpr -> Text
writeBodyExpr (AlphaNumBodyExpr ranged) = writeRanged ranged
writeBodyExpr (SpecBodyExpr ranged) = writeRanged ranged
writeBodyExpr (NumberBodyExpr ranged) = writeNumber ranged
writeBodyExpr (TextBodyExpr Ranged {rItem, rSpaces}) =
  let escapedText = pack $ reverse $ foldl (\acc c -> if c == '"' then c : c : acc else c : acc) "" $ unpack rItem
   in [st|#{rSpaces}"#{escapedText}"|]
writeBodyExpr (HigherPriorityExpr (IndentedNested expressions)) = mconcat $ map writeExprsBlock expressions
writeBodyExpr (HigherPriorityExpr (InBracketsNested {ibExprs = OcRanged {ocItem = expressions, oSpaces = oS, cSpaces = cS}, ibKind})) =
  let joinedExpressions = mconcat $ map writeExprsBlock expressions
   in case ibKind of
        RoundBrackets -> [st|#{oS}(#{joinedExpressions}#{cS})|]
        SquareBrackets -> [st|#{oS}[#{joinedExpressions}#{cS}]|]
        CurlyBrackets -> [st|#{oS}{#{joinedExpressions}#{cS}}|]
writeBodyExpr (UnderscoreBodyExpr r) = writeRangedWith (const "_") r

writeFunc :: Ranged Func -> Text
writeFunc Ranged {rItem = Func {fSig, fBody = FuncBody Ranged {rItem = expressions, rSpaces = bS}}, rSpaces} =
  let sigText = writeFuncSig fSig
      bodyText = mconcat $ map writeExprsBlock expressions
   in [st|#{rSpaces}#{sigText}#{bS}->#{bodyText}|]

writeFuncs :: [Ranged Func] -> Text
writeFuncs items = mconcat $ map writeFunc items
