module Pure.Main.Syntax.Lexer.LexerSourceTests
  ( lexerFormattedWritingTests,
  )
where

import Data.Either (Either (Left, Right))
import Data.Function (($))
import Data.Text (Text, unpack)
import Main.Syntax.Lexer.Parsing.Lexer (lexing)
import Main.Syntax.Lexer.Writing.Source (writeFuncs)
import Pure.Main.Syntax.Shared (runLexer)
import Shared.Errors (Error (Error), Errors (Errors))
import Shared.Text.Utils (withBorder)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Text.Shakespeare.Text (sbt, st)
import Text.Show (Show (show))

assertWrittenParsedEqualsSource :: Text -> Assertion
assertWrittenParsedEqualsSource source =
  let parsed = runLexer source (lexing "")
      written = case parsed of
        Left (Errors [Error error]) -> Left [st|"Error: '#{error}'"|]
        Left errors -> Left [st|"Errors: '#{show errors}'"|]
        Right (funcs, spaces) -> Right [st|#{writeFuncs funcs}#{spaces}|]
   in assertEqual "" (Right source) written

testCaseParseFormatted :: Text -> TestTree
testCaseParseFormatted source =
  testCase (unpack $ withBorder source) $ assertWrittenParsedEqualsSource source

lexerFormattedWritingTests :: TestTree
lexerFormattedWritingTests =
  testGroup
    "Syntax formatted writer tests"
    [ testCaseParseFormatted "functionName\t->\tid",
      testCaseParseFormatted "functionName parameter\t->\t5",
      testCaseParseFormatted "function1 (parameter) name\t->\t6.7",
      testCaseParseFormatted "function2_with_params (parametrized_param_one _) (parameterized_param_two _)\t->\t\"some text\"",
      testCaseParseFormatted "(parameter_one) => (parameter_two)\t->\t*:",
      testCaseParseFormatted "function3\t->\t->",
      testCaseParseFormatted "function4\t->\n\t1\t2\t3.4\t\"some text\"",
      testCaseParseFormatted "function5\t->\n\t1\t2\n\t\tsub1\t\tsub2\n\t3.4\t\"some text\"",
      testCaseParseFormatted "function6\t->\t...\tsome\tindex\ttype",
      testCaseParseFormatted "function7 ->;1;2;3.4;\"some text\"",
      testCaseParseFormatted "function8 ->  ; 1 ;    2; 3.4  ;",
      testCaseParseFormatted "function9 ->  ; 1 ;    ;;\n\t2",
      testCaseParseFormatted "function10 ->  ; 1 ;    ;;\n\t;\n\t2",
      testCaseParseFormatted "function11 ->  ; 1 ;    ;;\n\t;;\n\t2",
      testCaseParseFormatted "function12 ->  ; 1 ;    ;;\n\t;\n\t\n\t;;\n\t3.4 ",
      testCaseParseFormatted "function13 ->\n\t1\n\t[ ;]\n\t 2\n\t3",
      testCaseParseFormatted "function15\n\t->\n\t\ta\n\t;b\n\t\tc",
      testCaseParseFormatted "text_in_brackets_as_param (\"txt\")->1",
      testCaseParseFormatted "( 0.0e0) infix_function_with_numeric_params ( 2)  -> 3",
      testCaseParseFormatted "prefix_func_with_numeric_params 0.0   (  0.0)     0.0e0 ->4",
      testCaseParseFormatted "prefix_func_with_two_params    -4.0e-3 X -> 5",
      testCaseParseFormatted "func_having_text_as_param    \"text \"\"internal text in squares\"\" \" -> 5",
      testCaseParseFormatted "first_param + second_param -> x",
      testCaseParseFormatted "( first_param ) +(  second_param)-> x",
      testCaseParseFormatted "( first_param ) +   (  second_param) -> x ",
      testCaseParseFormatted "! x -> x",
      testCaseParseFormatted "1f x y -> x",
      testCaseParseFormatted "f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14-> x",
      testCaseParseFormatted "f -> x-y",
      testCaseParseFormatted "(left_param) plus (right_param ) -> x",
      testCaseParseFormatted "(left_param)plus(right_param)->x",
      testCaseParseFormatted "(   FIRST_ARG)   '(     \"'}''J'\")->        (        3e68)        \"'#''P''F'\"",
      testCaseParseFormatted "(  0_G )*<$  (        arg_2)->2",
      testCaseParseFormatted "(        0.0e0)          /(        TO)        ->3",
      testCaseParseFormatted "number_body_func ->      9e-10",
      testCaseParseFormatted "(\n\tr)  !/\n\t(\n\t6e-1)->\n\t4e-3\n\tJT      1.4e6\n\n\t1.5",
      testCaseParseFormatted "func1\t->\ta1\nfunc2\t->\n\t1\t2\nfunc3\t->\tf3",
      testCaseParseFormatted "func1\t->\n\ta1\ta2\nfunc2\t->\n\t3234\tsdfsdf\nfunc3\t->\n\tf3\te3",
      testCaseParseFormatted
        [sbt|
                                  |(first_parameter)
                                  | +
                                  | (second_parameter) -> ;
                                  |],
      testCaseParseFormatted
        [sbt|
            |while cond body -> 
            |	cond ?
            |		true -> 
            |			***body
            |			***cond
            |			while newCond body
            |		false -> _
            |	***
            |],
      testCaseParseFormatted "# some -> some",
      testCaseParseFormatted "#b -> t",
      testCaseParseFormatted "# some -> one | another",
      testCaseParseFormatted "# some -> \n\tone | \n\tanother",
      testCaseParseFormatted "# \n\tsome\n\tdata\n\ttype -> \n\tone | \n\tanother"
    ]
