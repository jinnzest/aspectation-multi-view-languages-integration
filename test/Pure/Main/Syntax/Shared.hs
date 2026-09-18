module Pure.Main.Syntax.Shared
  ( runLexer,
    mkRange,
    mkRanged,
    mkOcRanged,
    mkOcRangedWS,
    mkRangedWS,
  )
where

import Control.Monad.Except (runExceptT)
import Data.Either (Either)
import Data.Function (($))
import Data.Functor.Identity (Identity (runIdentity))
import Data.Int (Int)
import Data.Text (Text)
import Main.Syntax.Lexer.Parsing.LexerIO (lexingText)
import Shared.Errors (Errors)
import Shared.Lexer.Data (Lexer)
import Shared.Location.Data (OcRanged (OcRanged), Position (Position), Range (Range), Ranged (Ranged), cSpaces, column, from, line, oSpaces, ocItem, ocRange, rItem, rSpaces, range, to)

runLexer :: Text -> Lexer (a, Text) -> Either Errors (a, Text)
runLexer body lexer = runIdentity $ runExceptT $ lexingText "" body lexer

mkRangedWS :: Int -> Int -> Int -> Int -> Text -> a -> Ranged a
mkRangedWS lineFrom columnFrom lineTo columnTo rSpaces rItem =
  Ranged
    { rItem,
      range = mkRange lineFrom columnFrom lineTo columnTo,
      rSpaces
    }

mkRange :: Int -> Int -> Int -> Int -> Range
mkRange lineFrom columnFrom lineTo columnTo =
  Range
    { from = Position {line = lineFrom, column = columnFrom},
      to = Position {line = lineTo, column = columnTo}
    }

mkRanged :: Int -> Int -> Int -> Int -> a -> Ranged a
mkRanged lineFrom columnFrom lineTo columnTo = mkRangedWS lineFrom columnFrom lineTo columnTo ""

mkOcRangedWS :: Int -> Int -> Int -> Int -> Text -> Text -> a -> OcRanged a
mkOcRangedWS lineFrom columnFrom lineTo columnTo oSpaces cSpaces ocItem =
  OcRanged
    { ocItem,
      ocRange = mkRange lineFrom columnFrom lineTo columnTo,
      oSpaces,
      cSpaces
    }

mkOcRanged :: Int -> Int -> Int -> Int -> a -> OcRanged a
mkOcRanged lineFrom columnFrom lineTo columnTo = mkOcRangedWS lineFrom columnFrom lineTo columnTo "" ""
