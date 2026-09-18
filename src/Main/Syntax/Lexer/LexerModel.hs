{-# LANGUAGE GADTs #-}

module Main.Syntax.Lexer.LexerModel
  ( Func (Func, fBody, fSig),
    LexerModel,
    Number (Number, mant, dec, exp, sign),
    Nested (InBracketsNested, IndentedNested, ibExprs, ibKind),
    BodyExpr
      ( AlphaNumBodyExpr,
        SpecBodyExpr,
        NumberBodyExpr,
        UnderscoreBodyExpr,
        TextBodyExpr,
        HigherPriorityExpr
      ),
    ParamSigExpr
      ( AlphaNumParamSigExpr,
        NumberParamSigExpr,
        TextParamSigExpr,
        HpParamSigExpr
      ),
    NameSigExpr
      ( AlphaNumNameSigExpr,
        SpecNameSigExpr
      ),
    HpParamSigExpr (FoParamSigExpr, HoParamSigExpr, hpseName, hpseParams, hpseKind),
    HoParamParams (HoInfixParamParams, HoPrefixParamParams),
    ExprsBlock (ExprsBlock, ebExprs, ebSemicolon),
    FuncSig (FuncSig, fsName, fsParams, fsURL),
    FuncBody (FuncBody),
    FuncSigParams (PrefixSigParams, InfixSigParams),
  )
where

import Data.Bool (Bool)
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (Maybe)
import Data.Ord (Ord)
import Data.String (fromString)
import Data.Text (Text)
import Deriving.Aeson (CustomJSON (CustomJSON), Generic, ToJSON)
import Deriving.Aeson.Stock (Vanilla)
import GHC.Integer (Integer)
import Shared.Location.Data (BracketsKind, OcRanged, Ranged)
import Shared.ShowYaml (showYaml)
import Text.Shakespeare.Text as ST (ToText (toText))
import Text.Show (Show, show)

type Number :: Type
data Number = Number
  { mant :: Integer,
    dec :: Maybe Integer,
    exp :: Maybe Integer,
    sign :: Bool
  }
  deriving stock (Eq, Ord, Generic)
  deriving (ToJSON) via Vanilla Number

type ExprsBlock :: Type
data ExprsBlock = ExprsBlock {ebExprs :: [BodyExpr], ebSemicolon :: Maybe (Ranged ())}
  deriving stock (Eq, Ord, Generic)
  deriving (ToJSON) via Vanilla ExprsBlock

type Nested :: Type
data Nested = IndentedNested [ExprsBlock] | InBracketsNested {ibExprs :: OcRanged [ExprsBlock], ibKind :: BracketsKind}
  deriving stock (Eq, Ord, Generic)
  deriving (ToJSON) via Vanilla Nested

type BodyExpr :: Type
data BodyExpr
  = AlphaNumBodyExpr (Ranged Text)
  | SpecBodyExpr (Ranged Text)
  | NumberBodyExpr (Ranged Number)
  | UnderscoreBodyExpr (Ranged ())
  | TextBodyExpr (Ranged Text)
  | HigherPriorityExpr Nested
  deriving stock (Eq, Ord, Generic)
  deriving (ToJSON) via Vanilla BodyExpr

type HoParamParams :: Type
data HoParamParams = HoInfixParamParams (Ranged ()) (Ranged ()) | HoPrefixParamParams [Ranged ()]
  deriving stock (Eq, Ord, Generic)
  deriving (ToJSON) via Vanilla HoParamParams

type NameSigExpr :: Type
data NameSigExpr = AlphaNumNameSigExpr (Ranged Text) | SpecNameSigExpr (Ranged Text)
  deriving stock (Eq, Ord, Generic)
  deriving (ToJSON) via Vanilla NameSigExpr

type HpParamSigExpr :: Type
data HpParamSigExpr
  = HoParamSigExpr {hpseName :: NameSigExpr, hpseParams :: HoParamParams, hpseKind :: BracketsKind}
  | FoParamSigExpr ParamSigExpr
  deriving stock (Eq, Ord, Generic)
  deriving (ToJSON) via Vanilla HpParamSigExpr

type ParamSigExpr :: Type
data ParamSigExpr
  = AlphaNumParamSigExpr (Ranged Text)
  | NumberParamSigExpr (Ranged Number)
  | TextParamSigExpr (Ranged Text)
  | HpParamSigExpr (OcRanged HpParamSigExpr)
  deriving stock (Eq, Ord, Generic)
  deriving (ToJSON) via Vanilla ParamSigExpr

type FuncBody :: Type
newtype FuncBody = FuncBody (Ranged [ExprsBlock])
  deriving stock (Eq, Generic)
  deriving (ToJSON) via Vanilla FuncBody

type FuncSigParams :: Type
data FuncSigParams = PrefixSigParams [ParamSigExpr] | InfixSigParams ParamSigExpr ParamSigExpr
  deriving stock (Eq, Generic)
  deriving (ToJSON) via FuncSigParams

type FuncSig :: Type
data FuncSig = FuncSig
  { fsName :: NameSigExpr,
    fsParams :: FuncSigParams,
    fsURL :: Text
  }
  deriving stock (Eq, Generic)
  deriving (ToJSON) via Vanilla FuncSig

type Func :: Type
data Func = Func
  { fSig :: FuncSig,
    fBody :: FuncBody
  }
  deriving stock (Eq, Generic)
  deriving (ToJSON) via Vanilla Func

type LexerModel :: Type
type LexerModel = [Ranged Func]

instance Hashable Number

instance Hashable HoParamParams

instance Hashable HpParamSigExpr

instance Hashable ParamSigExpr

instance Hashable NameSigExpr

instance Hashable Nested

instance Hashable BodyExpr

instance Hashable ExprsBlock

instance Hashable FuncSigParams

instance Hashable FuncSig

instance Hashable FuncBody

instance Hashable Func

instance Show Number where
  show = showYaml

instance Show HpParamSigExpr where
  show = showYaml

instance Show ParamSigExpr where
  show = showYaml

instance Show NameSigExpr where
  show = showYaml

instance Show FuncSig where
  show = showYaml

instance Show BodyExpr where
  show = showYaml

instance Show ExprsBlock where
  show = showYaml

instance Show FuncBody where
  show = showYaml

instance Show Func where
  show = showYaml

instance Show Nested where
  show = showYaml

instance ToText Number where
  toText = fromString . showYaml

instance ToText (NonEmpty ExprsBlock) where
  toText = fromString . showYaml

instance ToText ParamSigExpr where
  toText = fromString . showYaml

instance ToText HpParamSigExpr where
  toText = fromString . showYaml

instance ToText ExprsBlock where
  toText = fromString . showYaml

instance ToText [ExprsBlock] where
  toText = fromString . showYaml

instance ToText FuncSig where
  toText = fromString . showYaml

instance ToText BodyExpr where
  toText = fromString . showYaml

instance ToText [BodyExpr] where
  toText = fromString . showYaml

instance ToText [[BodyExpr]] where
  toText = fromString . showYaml

instance ToText Func where
  toText = fromString . showYaml

instance ToText LexerModel where
  toText = fromString . showYaml
