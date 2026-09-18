module Pure.Main.Syntax.Lexer.Parsing.LexerErrorTests
  ( lexerErrorTests,
  )
where

import Data.Either (Either (Left, Right))
import Data.Function (($))
import Data.Text as T (Text, pack, unpack)
import Main.Syntax.Lexer.LexerModel (Func)
import Main.Syntax.Lexer.Parsing.Lexer (lexing)
import Main.Syntax.Lexer.Writing.Ranged (writeFuncs)
import Pure.Main.Syntax.Shared (runLexer)
import Shared.Errors (Error (Error), Errors (Errors, errors))
import Shared.Location.Data (Ranged)
import Shared.Text.Utils (nL, withBorder)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Text.Shakespeare.Text (sbt, st)
import Text.Show (Show (show))

alignUR :: [Ranged Func] -> Either Text Text
alignUR other = Left [st|#{writeFuncs other}#{nL}|]

assertParsedErrors :: Text -> Text -> Assertion
assertParsedErrors source expected =
  let parsedResult = runLexer source (lexing "")
      parsed = case parsedResult of
        Left (Errors {errors = [Error errorText]}) -> Right errorText
        Left unexpected -> Left $ T.pack $ show unexpected
        Right (other, _) -> alignUR other
   in assertEqual "" (Right expected) parsed

testCaseParseError :: Text -> Text -> TestTree
testCaseParseError source expected =
  testCase (unpack $ withBorder source) $ assertParsedErrors source expected

lexerErrorTests :: TestTree
lexerErrorTests =
  testGroup
    "Main syntax parser error test"
    [ testCaseParseError
        "nameOnly"
        [sbt|1:9:
             |  |
             |1 | nameOnly
             |  |         ^
             |unexpected end of input
             |expecting "->", alphanumeric identifier, carriage return, newline, number, open bracket, space, tab, or text
             |],
      testCaseParseError
        " 1"
        [sbt|1:2:
             |  |
             |1 |  1
             |  |  ^
             |incorrect indentation (got 2, should be equal to 1)
             |],
      testCaseParseError
        "noBodyFunc ->"
        [sbt|1:14:
             |  |
             |1 | noBodyFunc ->
             |  |              ^
             |unexpected end of input
             |expecting alphanumeric identifier, carriage return, newline, number, open bracket, space, special characters identifier, tab, text, or underscore
             |],
      testCaseParseError
        "brokenBodyFunc->("
        [sbt|1:18:
             |  |
             |1 | brokenBodyFunc->(
             |  |                  ^
             |unexpected end of input
             |expecting alphanumeric identifier, carriage return, close bracket, newline, number, open bracket, space, special characters identifier, tab, text, or underscore
             |],
      testCaseParseError
        "f->(123456789"
        [sbt|1:14:
             |  |
             |1 | f->(123456789
             |  |              ^
             |unexpected end of input
             |expecting alphanumeric identifier, carriage return, close bracket, newline, number, open bracket, space, special characters identifier, tab, text, or underscore
             |],
      testCaseParseError
        "f->)"
        [sbt|1:4:
             |  |
             |1 | f->)
             |  |    ^
             |unexpected ')'
             |expecting alphanumeric identifier, carriage return, newline, number, open bracket, space, special characters identifier, tab, text, or underscore
             |],
      testCaseParseError
        "+"
        [sbt|1:2:
             |  |
             |1 | +
             |  |  ^
             |unexpected end of input
             |expecting "->", alphanumeric identifier, carriage return, newline, number, open bracket, space, tab, or text
             |],
      testCaseParseError
        "(xyz->1"
        [sbt|1:5:
             |  |
             |1 | (xyz->1
             |  |     ^
             |unexpected '-'
             |expecting carriage return, close bracket, newline, space, or tab
             |],
      testCaseParseError
        "->123f"
        [sbt|1:3:
             |  |
             |1 | ->123f
             |  |   ^
             |"->" is not allowed inside function signature
             |],
      testCaseParseError
        "#"
        [sbt|1:2:
             |  |
             |1 | ##
             |  |  ^
             |unexpected end of input
             |expecting "->", alphanumeric identifier, carriage return, newline, number, open bracket, space, tab, or text
             |],
      testCaseParseError
        " x"
        [sbt|1:2:
             |  |
             |1 |  x
             |  |  ^
             |incorrect indentation (got 2, should be equal to 1)
             |],
      testCaseParseError
        "f (_ _)"
        [sbt|1:3:
             |  |
             |1 | f (_ _)
             |  |   ^^
             |unexpected "(_"
             |expecting "->", carriage return, newline, space, or tab
             |],
      testCaseParseError
        "f (_ fi)"
        [sbt|1:3:
            |  |
            |1 | f (_ fi)
            |  |   ^^
            |unexpected "(_"
            |expecting "->", carriage return, newline, space, or tab
            |],
      testCaseParseError
        "_ -> 1"
        [sbt|1:2:
             |  |
             |1 | _ -> 1
             |  |  ^
             |alphanumeric can't be a single underscore
             |],
      testCaseParseError
        "#"
        [sbt|1:2:
             |  |
             |1 | ##
             |  |  ^
             |unexpected end of input
             |expecting "->", alphanumeric identifier, carriage return, newline, number, open bracket, space, tab, or text
             |],
      testCaseParseError
        "# name"
        [sbt|1:7:
             |  |
             |1 | # name
             |  |       ^
             |unexpected end of input
             |expecting "->", alphanumeric identifier, carriage return, newline, number, open bracket, space, tab, or text
             |],
      testCaseParseError
        "# +"
        [sbt|1:3:
             |  |
             |1 | # +
             |  |   ^
             |unexpected '+'
             |expecting "->", carriage return, newline, space, or tab
             |],
      testCaseParseError
        "# name ->"
        [sbt|1:10:
             |  |
             |1 | # name ->
             |  |          ^
             |unexpected end of input
             |expecting alphanumeric identifier, carriage return, newline, number, open bracket, space, special characters identifier, tab, text, or underscore
             |]
    ]
