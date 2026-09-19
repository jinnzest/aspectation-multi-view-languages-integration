module Pure.Main.Syntax.Lexer.Parsing.LexerTests
  ( lexerTests,
  )
where

import Data.Either (Either (Left, Right))
import Data.Function (($))
import Data.Text (Text, unpack)
import Main.Syntax.Lexer.LexerModel (Func)
import Main.Syntax.Lexer.Parsing.Lexer (lexing)
import Main.Syntax.Lexer.Writing.Ranged (writeFuncs)
import Pure.Main.Syntax.Shared (runLexer)
import Shared.Errors (Error (Error), Errors (Errors))
import Shared.Location.Data (Ranged)
import Shared.Text.Utils (nL, withBorder)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Text.Shakespeare.Text (sbt, st)
import Text.Show (Show (show))

alignAR :: [Ranged Func] -> Either a Text
alignAR other = Right [st|#{writeFuncs other}#{nL}|]

assertParsedEqualsSource :: Text -> Text -> Assertion
assertParsedEqualsSource source expected =
  let parsedResult = runLexer source (lexing "")
      parsed = case parsedResult of
        Left (Errors [Error error]) -> Left [st|"Error: '#{error}'"|]
        Left errors -> Left [st|"Errors: '#{show errors}'"|]
        Right (other, _) -> alignAR other
   in assertEqual "" (Right expected) parsed

testCaseParse :: Text -> Text -> TestTree
testCaseParse source expected =
  let borderedSource = unpack $ withBorder source
   in testCase borderedSource $ assertParsedEqualsSource source expected

lexerTests :: TestTree
lexerTests =
  testGroup
    "Lexer test"
    [ testCaseParse
        "emptyFunc1 -> _"
        [sbt|
            |1:1 - 1:16 function
            |	1:1 - 1:11 prefix function signature
            |		1:1 - 1:11 name
            |			1:1 - 1:11 alphnumeric identifier
            |				emptyFunc1
            |	1:12 - 1:16 function body
            |		1:15 - 1:16 expressions block
            |			1:15 - 1:16 underscore
            |],
      testCaseParse
        "emptyFunc2 -> (_)"
        [sbt|
            |1:1 - 1:18 function
            |	1:1 - 1:11 prefix function signature
            |		1:1 - 1:11 name
            |			1:1 - 1:11 alphnumeric identifier
            |				emptyFunc2
            |	1:12 - 1:18 function body
            |		1:15 - 1:18 expressions block
            |			1:15 - 1:18 higher priority expressions
            |				1:16 - 1:17 expressions block
            |					1:16 - 1:17 underscore
            |],
      testCaseParse
        "func99 -> 2"
        [sbt|
            |1:1 - 1:12 function
            |	1:1 - 1:7 prefix function signature
            |		1:1 - 1:7 name
            |			1:1 - 1:7 alphnumeric identifier
            |				func99
            |	1:8 - 1:12 function body
            |		1:11 - 1:12 expressions block
            |			1:11 - 1:12 number
            |				integer
            |					2
            |],
      testCaseParse
        "func1 -> 1\nfunc2->2\nfunc3   ->   3  "
        [sbt|
            |1:1 - 1:11 function
            |	1:1 - 1:6 prefix function signature
            |		1:1 - 1:6 name
            |			1:1 - 1:6 alphnumeric identifier
            |				func1
            |	1:7 - 1:11 function body
            |		1:10 - 1:11 expressions block
            |			1:10 - 1:11 number
            |				integer
            |					1
            |2:1 - 2:9 function
            |	2:1 - 2:6 prefix function signature
            |		2:1 - 2:6 name
            |			2:1 - 2:6 alphnumeric identifier
            |				func2
            |	2:6 - 2:9 function body
            |		2:8 - 2:9 expressions block
            |			2:8 - 2:9 number
            |				integer
            |					2
            |3:1 - 3:15 function
            |	3:1 - 3:6 prefix function signature
            |		3:1 - 3:6 name
            |			3:1 - 3:6 alphnumeric identifier
            |				func3
            |	3:9 - 3:15 function body
            |		3:14 - 3:15 expressions block
            |			3:14 - 3:15 number
            |				integer
            |					3
            |],
      testCaseParse
        "func1 1arg 2arg arg3-> 4arg"
        [sbt|
            |1:1 - 1:28 function
            |	1:1 - 1:6 prefix function signature
            |		1:1 - 1:6 name
            |			1:1 - 1:6 alphnumeric identifier
            |				func1
            |		1:7 - 1:21 parameters
            |			1:7 - 1:11 parameter 1
            |				1:7 - 1:11 alphnumeric identifier
            |					1arg
            |			1:12 - 1:16 parameter 2
            |				1:12 - 1:16 alphnumeric identifier
            |					2arg
            |			1:17 - 1:21 parameter 3
            |				1:17 - 1:21 alphnumeric identifier
            |					arg3
            |	1:21 - 1:28 function body
            |		1:24 - 1:28 expressions block
            |			1:24 - 1:28 alphnumeric identifier
            |				4arg
            |],
      testCaseParse
        "(first_arg) = (second_arg) -> *:"
        [sbt|
            |1:1 - 1:33 function
            |	1:13 - 1:14 infix function signature
            |		1:13 - 1:14 name
            |			1:13 - 1:14 special identifier
            |				=
            |		1:1 - 1:27 parameters
            |			1:1 - 1:12 left parameter 
            |				1:1 - 1:12 higher priority parameter
            |					first order parameter
            |						1:2 - 1:11 alphnumeric identifier
            |							first_arg
            |			1:15 - 1:27 right parameter 
            |				1:15 - 1:27 higher priority parameter
            |					first order parameter
            |						1:16 - 1:26 alphnumeric identifier
            |							second_arg
            |	1:28 - 1:33 function body
            |		1:31 - 1:33 expressions block
            |			1:31 - 1:33 special identifier
            |				*:
            |],
      testCaseParse
        "func_hp->expr1 (higher priority expr) expr2"
        [sbt|
            |1:1 - 1:44 function
            |	1:1 - 1:8 prefix function signature
            |		1:1 - 1:8 name
            |			1:1 - 1:8 alphnumeric identifier
            |				func_hp
            |	1:8 - 1:44 function body
            |		1:10 - 1:44 expressions block
            |			1:10 - 1:15 alphnumeric identifier
            |				expr1
            |			1:16 - 1:38 higher priority expressions
            |				1:17 - 1:37 expressions block
            |					1:17 - 1:23 alphnumeric identifier
            |						higher
            |					1:24 - 1:32 alphnumeric identifier
            |						priority
            |					1:33 - 1:37 alphnumeric identifier
            |						expr
            |			1:39 - 1:44 alphnumeric identifier
            |				expr2
            |],
      testCaseParse
        "func15->\n\texpr1\n\texpr2"
        [sbt|
            |1:1 - 3:14 function
            |	1:1 - 1:7 prefix function signature
            |		1:1 - 1:7 name
            |			1:1 - 1:7 alphnumeric identifier
            |				func15
            |	1:7 - 3:14 function body
            |		2:9 - 2:14 expressions block
            |			2:9 - 2:14 alphnumeric identifier
            |				expr1
            |		3:9 - 3:14 expressions block
            |			3:9 - 3:14 alphnumeric identifier
            |				expr2
            |],
      testCaseParse
        "func16->\n\texpr1\n\t1.2\n\t\tnested expression1\n\t\tnested expression2\n\texpr2\n\t\tnested expression3\n\t\tnested expression4\n\texpr3\n\ta->b"
        [sbt|
            |1:1 - 10:13 function
            |	1:1 - 1:7 prefix function signature
            |		1:1 - 1:7 name
            |			1:1 - 1:7 alphnumeric identifier
            |				func16
            |	1:7 - 10:13 function body
            |		2:9 - 2:14 expressions block
            |			2:9 - 2:14 alphnumeric identifier
            |				expr1
            |		3:9 - 5:35 expressions block
            |			3:9 - 3:12 number
            |				integer
            |					1
            |				decimal
            |					2
            |			4:17 - 5:35 nested expressions
            |				4:17 - 4:35 expressions block
            |					4:17 - 4:23 alphnumeric identifier
            |						nested
            |					4:24 - 4:35 alphnumeric identifier
            |						expression1
            |				5:17 - 5:35 expressions block
            |					5:17 - 5:23 alphnumeric identifier
            |						nested
            |					5:24 - 5:35 alphnumeric identifier
            |						expression2
            |		6:9 - 8:35 expressions block
            |			6:9 - 6:14 alphnumeric identifier
            |				expr2
            |			7:17 - 8:35 nested expressions
            |				7:17 - 7:35 expressions block
            |					7:17 - 7:23 alphnumeric identifier
            |						nested
            |					7:24 - 7:35 alphnumeric identifier
            |						expression3
            |				8:17 - 8:35 expressions block
            |					8:17 - 8:23 alphnumeric identifier
            |						nested
            |					8:24 - 8:35 alphnumeric identifier
            |						expression4
            |		9:9 - 9:14 expressions block
            |			9:9 - 9:14 alphnumeric identifier
            |				expr3
            |		10:9 - 10:13 expressions block
            |			10:9 - 10:10 alphnumeric identifier
            |				a
            |			10:10 - 10:12 special identifier
            |				->
            |			10:12 - 10:13 alphnumeric identifier
            |				b
            |],
      testCaseParse
        "func->\n\texpr1\n\t\tnested expression 1\n\t\tnested expression 2\n\texpr2 expr3\n\texpr4"
        [sbt|
            |1:1 - 6:14 function
            |	1:1 - 1:5 prefix function signature
            |		1:1 - 1:5 name
            |			1:1 - 1:5 alphnumeric identifier
            |				func
            |	1:5 - 6:14 function body
            |		2:9 - 4:36 expressions block
            |			2:9 - 2:14 alphnumeric identifier
            |				expr1
            |			3:17 - 4:36 nested expressions
            |				3:17 - 3:36 expressions block
            |					3:17 - 3:23 alphnumeric identifier
            |						nested
            |					3:24 - 3:34 alphnumeric identifier
            |						expression
            |					3:35 - 3:36 number
            |						integer
            |							1
            |				4:17 - 4:36 expressions block
            |					4:17 - 4:23 alphnumeric identifier
            |						nested
            |					4:24 - 4:34 alphnumeric identifier
            |						expression
            |					4:35 - 4:36 number
            |						integer
            |							2
            |		5:9 - 5:20 expressions block
            |			5:9 - 5:14 alphnumeric identifier
            |				expr2
            |			5:15 - 5:20 alphnumeric identifier
            |				expr3
            |		6:9 - 6:14 expressions block
            |			6:9 - 6:14 alphnumeric identifier
            |				expr4
            |],
      testCaseParse
        "nested_semicolons1->;expr1;expr2"
        [sbt|
            |1:1 - 1:33 function
            |	1:1 - 1:19 prefix function signature
            |		1:1 - 1:19 name
            |			1:1 - 1:19 alphnumeric identifier
            |				nested_semicolons1
            |	1:19 - 1:33 function body
            |		1:21 - 1:27 expressions block
            |			1:21 - 1:22 semicolon
            |			1:22 - 1:27 expressions
            |				1:22 - 1:27 alphnumeric identifier
            |					expr1
            |		1:27 - 1:33 expressions block
            |			1:27 - 1:28 semicolon
            |			1:28 - 1:33 expressions
            |				1:28 - 1:33 alphnumeric identifier
            |					expr2
            |],
      testCaseParse
        "nested_semicolons2 -> expr1;expr2"
        [sbt|
            |1:1 - 1:34 function
            |	1:1 - 1:19 prefix function signature
            |		1:1 - 1:19 name
            |			1:1 - 1:19 alphnumeric identifier
            |				nested_semicolons2
            |	1:20 - 1:34 function body
            |		1:23 - 1:28 expressions block
            |			1:23 - 1:28 alphnumeric identifier
            |				expr1
            |		1:28 - 1:34 expressions block
            |			1:28 - 1:29 semicolon
            |			1:29 - 1:34 expressions
            |				1:29 - 1:34 alphnumeric identifier
            |					expr2
            |],
      testCaseParse
        "nested_semicolons3 -> ;expr1 ; expr2 expr3 ; expr4"
        [sbt|
            |1:1 - 1:51 function
            |	1:1 - 1:19 prefix function signature
            |		1:1 - 1:19 name
            |			1:1 - 1:19 alphnumeric identifier
            |				nested_semicolons3
            |	1:20 - 1:51 function body
            |		1:23 - 1:29 expressions block
            |			1:23 - 1:24 semicolon
            |			1:24 - 1:29 expressions
            |				1:24 - 1:29 alphnumeric identifier
            |					expr1
            |		1:30 - 1:43 expressions block
            |			1:30 - 1:31 semicolon
            |			1:32 - 1:43 expressions
            |				1:32 - 1:37 alphnumeric identifier
            |					expr2
            |				1:38 - 1:43 alphnumeric identifier
            |					expr3
            |		1:44 - 1:51 expressions block
            |			1:44 - 1:45 semicolon
            |			1:46 - 1:51 expressions
            |				1:46 - 1:51 alphnumeric identifier
            |					expr4
            |],
      testCaseParse
        "nested_semicolons4 ->;expr1 expr2"
        [sbt|
            |1:1 - 1:34 function
            |	1:1 - 1:19 prefix function signature
            |		1:1 - 1:19 name
            |			1:1 - 1:19 alphnumeric identifier
            |				nested_semicolons4
            |	1:20 - 1:34 function body
            |		1:22 - 1:34 expressions block
            |			1:22 - 1:23 semicolon
            |			1:23 - 1:34 expressions
            |				1:23 - 1:28 alphnumeric identifier
            |					expr1
            |				1:29 - 1:34 alphnumeric identifier
            |					expr2
            |],
      testCaseParse
        "nested_semicolons5 -> expr1 expr2;expr3"
        [sbt|
            |1:1 - 1:40 function
            |	1:1 - 1:19 prefix function signature
            |		1:1 - 1:19 name
            |			1:1 - 1:19 alphnumeric identifier
            |				nested_semicolons5
            |	1:20 - 1:40 function body
            |		1:23 - 1:34 expressions block
            |			1:23 - 1:28 alphnumeric identifier
            |				expr1
            |			1:29 - 1:34 alphnumeric identifier
            |				expr2
            |		1:34 - 1:40 expressions block
            |			1:34 - 1:35 semicolon
            |			1:35 - 1:40 expressions
            |				1:35 - 1:40 alphnumeric identifier
            |					expr3
            |],
      testCaseParse
        "nested_semicolons6 -> a\n\t\t\t;\n\t;\n\tb"
        [sbt|
            |1:1 - 4:10 function
            |	1:1 - 1:19 prefix function signature
            |		1:1 - 1:19 name
            |			1:1 - 1:19 alphnumeric identifier
            |				nested_semicolons6
            |	1:20 - 4:10 function body
            |		1:23 - 4:10 expressions block
            |			1:23 - 1:24 alphnumeric identifier
            |				a
            |			2:25 - 4:10 nested expressions
            |				2:25 - 3:10 expressions block
            |					2:25 - 2:26 nested expressions
            |						2:25 - 2:26 expressions block
            |							2:25 - 2:26 special identifier
            |								;
            |					3:9 - 3:10 special identifier
            |						;
            |				4:9 - 4:10 expressions block
            |					4:9 - 4:10 alphnumeric identifier
            |						b
            |],
      testCaseParse
        "nested_semicolons7 ->\n\t1\n\t[ ;]\n\t 2\n\t3"
        [sbt|
            |1:1 - 5:10 function
            |	1:1 - 1:19 prefix function signature
            |		1:1 - 1:19 name
            |			1:1 - 1:19 alphnumeric identifier
            |				nested_semicolons7
            |	1:20 - 5:10 function body
            |		2:9 - 2:10 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |		3:9 - 4:11 expressions block
            |			3:9 - 3:13 higher priority expressions
            |				3:11 - 3:12 expressions block
            |					3:11 - 3:12 semicolon
            |					3:11 - 3:12 empty nested
            |			4:10 - 4:11 nested expressions
            |				4:10 - 4:11 expressions block
            |					4:10 - 4:11 number
            |						integer
            |							2
            |		5:9 - 5:10 expressions block
            |			5:9 - 5:10 number
            |				integer
            |					3
            |],
      testCaseParse
        "nested_semicolons8 ->\n\n\t\"txt\"\n\n\t;2e-4\n\n\t\t\n\tKCZ2e-4"
        [sbt|
            |1:1 - 8:16 function
            |	1:1 - 1:19 prefix function signature
            |		1:1 - 1:19 name
            |			1:1 - 1:19 alphnumeric identifier
            |				nested_semicolons8
            |	1:20 - 8:16 function body
            |		3:9 - 3:14 expressions block
            |			3:9 - 3:14 text
            |				txt
            |		5:9 - 5:14 expressions block
            |			5:9 - 5:10 semicolon
            |			5:10 - 5:14 expressions
            |				5:10 - 5:14 number
            |					integer
            |						2
            |					exponent
            |						-4
            |		8:9 - 8:16 expressions block
            |			8:9 - 8:14 alphnumeric identifier
            |				KCZ2e
            |			8:14 - 8:16 number
            |				integer
            |					4
            |				signed
            |],
      testCaseParse
        "nested_semicolons9 ->\n\t\ta\n\t;b\n\t\tc"
        [sbt|
            |1:1 - 4:18 function
            |	1:1 - 1:19 prefix function signature
            |		1:1 - 1:19 name
            |			1:1 - 1:19 alphnumeric identifier
            |				nested_semicolons9
            |	1:20 - 4:18 function body
            |		2:17 - 2:18 expressions block
            |			2:17 - 2:18 nested expressions
            |				2:17 - 2:18 expressions block
            |					2:17 - 2:18 alphnumeric identifier
            |						a
            |		3:9 - 4:18 expressions block
            |			3:9 - 3:10 semicolon
            |			3:10 - 4:18 expressions
            |				3:10 - 3:11 alphnumeric identifier
            |					b
            |				4:17 - 4:18 nested expressions
            |					4:17 - 4:18 expressions block
            |						4:17 - 4:18 alphnumeric identifier
            |							c
            |],
      testCaseParse
        "nested_semicolons10 ->expr1;expr2;"
        [sbt|
            |1:1 - 1:35 function
            |	1:1 - 1:20 prefix function signature
            |		1:1 - 1:20 name
            |			1:1 - 1:20 alphnumeric identifier
            |				nested_semicolons10
            |	1:21 - 1:35 function body
            |		1:23 - 1:28 expressions block
            |			1:23 - 1:28 alphnumeric identifier
            |				expr1
            |		1:28 - 1:34 expressions block
            |			1:28 - 1:29 semicolon
            |			1:29 - 1:34 expressions
            |				1:29 - 1:34 alphnumeric identifier
            |					expr2
            |		1:34 - 1:35 expressions block
            |			1:34 - 1:35 semicolon
            |			1:34 - 1:35 empty nested
            |],
      testCaseParse
        "func->id1+++id2"
        [sbt|
            |1:1 - 1:16 function
            |	1:1 - 1:5 prefix function signature
            |		1:1 - 1:5 name
            |			1:1 - 1:5 alphnumeric identifier
            |				func
            |	1:5 - 1:16 function body
            |		1:7 - 1:16 expressions block
            |			1:7 - 1:10 alphnumeric identifier
            |				id1
            |			1:10 - 1:13 special identifier
            |				+++
            |			1:13 - 1:16 alphnumeric identifier
            |				id2
            |],
      testCaseParse
        "f -> (xV7 ...YL MG 68185085017189.7621760004214).CEp dsf"
        [sbt|
            |1:1 - 1:57 function
            |	1:1 - 1:2 prefix function signature
            |		1:1 - 1:2 name
            |			1:1 - 1:2 alphnumeric identifier
            |				f
            |	1:3 - 1:57 function body
            |		1:6 - 1:57 expressions block
            |			1:6 - 1:49 higher priority expressions
            |				1:7 - 1:48 expressions block
            |					1:7 - 1:10 alphnumeric identifier
            |						xV7
            |					1:11 - 1:14 special identifier
            |						...
            |					1:14 - 1:16 alphnumeric identifier
            |						YL
            |					1:17 - 1:19 alphnumeric identifier
            |						MG
            |					1:20 - 1:48 number
            |						integer
            |							68185085017189
            |						decimal
            |							7621760004214
            |			1:49 - 1:50 special identifier
            |				.
            |			1:50 - 1:53 alphnumeric identifier
            |				CEp
            |			1:54 - 1:57 alphnumeric identifier
            |				dsf
            |],
      testCaseParse
        "nestingBase -> \n\t1\n\t\t2"
        [sbt|
            |1:1 - 3:18 function
            |	1:1 - 1:12 prefix function signature
            |		1:1 - 1:12 name
            |			1:1 - 1:12 alphnumeric identifier
            |				nestingBase
            |	1:13 - 3:18 function body
            |		2:9 - 3:18 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			3:17 - 3:18 nested expressions
            |				3:17 - 3:18 expressions block
            |					3:17 - 3:18 number
            |						integer
            |							2
            |],
      testCaseParse
        "nestingBase2 -> \n\t\t1\n\t2"
        [sbt|
            |1:1 - 3:10 function
            |	1:1 - 1:13 prefix function signature
            |		1:1 - 1:13 name
            |			1:1 - 1:13 alphnumeric identifier
            |				nestingBase2
            |	1:14 - 3:10 function body
            |		2:17 - 3:10 expressions block
            |			2:17 - 2:18 nested expressions
            |				2:17 - 2:18 expressions block
            |					2:17 - 2:18 number
            |						integer
            |							1
            |			3:9 - 3:10 number
            |				integer
            |					2
            |],
      testCaseParse
        "nesting1 -> \n\t1\n\t\t2\n\t3"
        [sbt|
            |1:1 - 4:10 function
            |	1:1 - 1:9 prefix function signature
            |		1:1 - 1:9 name
            |			1:1 - 1:9 alphnumeric identifier
            |				nesting1
            |	1:10 - 4:10 function body
            |		2:9 - 3:18 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			3:17 - 3:18 nested expressions
            |				3:17 - 3:18 expressions block
            |					3:17 - 3:18 number
            |						integer
            |							2
            |		4:9 - 4:10 expressions block
            |			4:9 - 4:10 number
            |				integer
            |					3
            |],
      testCaseParse
        "nesting2 -> \n\t1\n\t\t2 3\n\t4"
        [sbt|
            |1:1 - 4:10 function
            |	1:1 - 1:9 prefix function signature
            |		1:1 - 1:9 name
            |			1:1 - 1:9 alphnumeric identifier
            |				nesting2
            |	1:10 - 4:10 function body
            |		2:9 - 3:20 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			3:17 - 3:20 nested expressions
            |				3:17 - 3:20 expressions block
            |					3:17 - 3:18 number
            |						integer
            |							2
            |					3:19 - 3:20 number
            |						integer
            |							3
            |		4:9 - 4:10 expressions block
            |			4:9 - 4:10 number
            |				integer
            |					4
            |],
      testCaseParse
        "nesting3 -> \n\t1\n\t\t2 3 4\n\t5"
        [sbt|
            |1:1 - 4:10 function
            |	1:1 - 1:9 prefix function signature
            |		1:1 - 1:9 name
            |			1:1 - 1:9 alphnumeric identifier
            |				nesting3
            |	1:10 - 4:10 function body
            |		2:9 - 3:22 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			3:17 - 3:22 nested expressions
            |				3:17 - 3:22 expressions block
            |					3:17 - 3:18 number
            |						integer
            |							2
            |					3:19 - 3:20 number
            |						integer
            |							3
            |					3:21 - 3:22 number
            |						integer
            |							4
            |		4:9 - 4:10 expressions block
            |			4:9 - 4:10 number
            |				integer
            |					5
            |],
      testCaseParse
        "nesting4 -> \n\t1\n\t\t\t\t\t\t2\n\t3"
        [sbt|
            |1:1 - 4:10 function
            |	1:1 - 1:9 prefix function signature
            |		1:1 - 1:9 name
            |			1:1 - 1:9 alphnumeric identifier
            |				nesting4
            |	1:10 - 4:10 function body
            |		2:9 - 3:50 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			3:49 - 3:50 nested expressions
            |				3:49 - 3:50 expressions block
            |					3:49 - 3:50 number
            |						integer
            |							2
            |		4:9 - 4:10 expressions block
            |			4:9 - 4:10 number
            |				integer
            |					3
            |],
      testCaseParse
        "nesting5 -> \n\t1\n\t\t2 3\n\t\t4 5\n\t6"
        [sbt|
            |1:1 - 5:10 function
            |	1:1 - 1:9 prefix function signature
            |		1:1 - 1:9 name
            |			1:1 - 1:9 alphnumeric identifier
            |				nesting5
            |	1:10 - 5:10 function body
            |		2:9 - 4:20 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			3:17 - 4:20 nested expressions
            |				3:17 - 3:20 expressions block
            |					3:17 - 3:18 number
            |						integer
            |							2
            |					3:19 - 3:20 number
            |						integer
            |							3
            |				4:17 - 4:20 expressions block
            |					4:17 - 4:18 number
            |						integer
            |							4
            |					4:19 - 4:20 number
            |						integer
            |							5
            |		5:9 - 5:10 expressions block
            |			5:9 - 5:10 number
            |				integer
            |					6
            |],
      testCaseParse
        "nesting6 -> \n\t1\n\t\t2 3 4\n\t\t5 6 7\n\t8"
        [sbt|
            |1:1 - 5:10 function
            |	1:1 - 1:9 prefix function signature
            |		1:1 - 1:9 name
            |			1:1 - 1:9 alphnumeric identifier
            |				nesting6
            |	1:10 - 5:10 function body
            |		2:9 - 4:22 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			3:17 - 4:22 nested expressions
            |				3:17 - 3:22 expressions block
            |					3:17 - 3:18 number
            |						integer
            |							2
            |					3:19 - 3:20 number
            |						integer
            |							3
            |					3:21 - 3:22 number
            |						integer
            |							4
            |				4:17 - 4:22 expressions block
            |					4:17 - 4:18 number
            |						integer
            |							5
            |					4:19 - 4:20 number
            |						integer
            |							6
            |					4:21 - 4:22 number
            |						integer
            |							7
            |		5:9 - 5:10 expressions block
            |			5:9 - 5:10 number
            |				integer
            |					8
            |],
      testCaseParse
        "nesting7 -> \n\t1\n\t\t2\n\t3\n\t\t4\n\t5\n\t\t6\n\t7"
        [sbt|
            |1:1 - 8:10 function
            |	1:1 - 1:9 prefix function signature
            |		1:1 - 1:9 name
            |			1:1 - 1:9 alphnumeric identifier
            |				nesting7
            |	1:10 - 8:10 function body
            |		2:9 - 3:18 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			3:17 - 3:18 nested expressions
            |				3:17 - 3:18 expressions block
            |					3:17 - 3:18 number
            |						integer
            |							2
            |		4:9 - 5:18 expressions block
            |			4:9 - 4:10 number
            |				integer
            |					3
            |			5:17 - 5:18 nested expressions
            |				5:17 - 5:18 expressions block
            |					5:17 - 5:18 number
            |						integer
            |							4
            |		6:9 - 7:18 expressions block
            |			6:9 - 6:10 number
            |				integer
            |					5
            |			7:17 - 7:18 nested expressions
            |				7:17 - 7:18 expressions block
            |					7:17 - 7:18 number
            |						integer
            |							6
            |		8:9 - 8:10 expressions block
            |			8:9 - 8:10 number
            |				integer
            |					7
            |],
      testCaseParse
        "nesting8 -> \n\t1\n\t\t2 3 4\n\t5\n\t\t6 7 8\n\t9\n\t\t10 11 12\n\t13"
        [sbt|
            |1:1 - 8:11 function
            |	1:1 - 1:9 prefix function signature
            |		1:1 - 1:9 name
            |			1:1 - 1:9 alphnumeric identifier
            |				nesting8
            |	1:10 - 8:11 function body
            |		2:9 - 3:22 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			3:17 - 3:22 nested expressions
            |				3:17 - 3:22 expressions block
            |					3:17 - 3:18 number
            |						integer
            |							2
            |					3:19 - 3:20 number
            |						integer
            |							3
            |					3:21 - 3:22 number
            |						integer
            |							4
            |		4:9 - 5:22 expressions block
            |			4:9 - 4:10 number
            |				integer
            |					5
            |			5:17 - 5:22 nested expressions
            |				5:17 - 5:22 expressions block
            |					5:17 - 5:18 number
            |						integer
            |							6
            |					5:19 - 5:20 number
            |						integer
            |							7
            |					5:21 - 5:22 number
            |						integer
            |							8
            |		6:9 - 7:25 expressions block
            |			6:9 - 6:10 number
            |				integer
            |					9
            |			7:17 - 7:25 nested expressions
            |				7:17 - 7:25 expressions block
            |					7:17 - 7:19 number
            |						integer
            |							10
            |					7:20 - 7:22 number
            |						integer
            |							11
            |					7:23 - 7:25 number
            |						integer
            |							12
            |		8:9 - 8:11 expressions block
            |			8:9 - 8:11 number
            |				integer
            |					13
            |],
      testCaseParse
        "nesting9 -> \n\t1\n\t\t2\n\t\t\t3\n\t\t\t\t4\n\t\t\t\t\t5"
        [sbt|
            |1:1 - 6:42 function
            |	1:1 - 1:9 prefix function signature
            |		1:1 - 1:9 name
            |			1:1 - 1:9 alphnumeric identifier
            |				nesting9
            |	1:10 - 6:42 function body
            |		2:9 - 6:42 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			3:17 - 6:42 nested expressions
            |				3:17 - 6:42 expressions block
            |					3:17 - 3:18 number
            |						integer
            |							2
            |					4:25 - 6:42 nested expressions
            |						4:25 - 6:42 expressions block
            |							4:25 - 4:26 number
            |								integer
            |									3
            |							5:33 - 6:42 nested expressions
            |								5:33 - 6:42 expressions block
            |									5:33 - 5:34 number
            |										integer
            |											4
            |									6:41 - 6:42 nested expressions
            |										6:41 - 6:42 expressions block
            |											6:41 - 6:42 number
            |												integer
            |													5
            |],
      testCaseParse
        "nesting10 -> \n\t\t\t\t\t1\n\t\t\t\t2\n\t\t\t3\n\t\t4\n\t5"
        [sbt|
            |1:1 - 6:10 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting10
            |	1:11 - 6:10 function body
            |		2:41 - 6:10 expressions block
            |			2:41 - 5:18 nested expressions
            |				2:41 - 5:18 expressions block
            |					2:41 - 4:26 nested expressions
            |						2:41 - 4:26 expressions block
            |							2:41 - 3:34 nested expressions
            |								2:41 - 3:34 expressions block
            |									2:41 - 2:42 nested expressions
            |										2:41 - 2:42 expressions block
            |											2:41 - 2:42 number
            |												integer
            |													1
            |									3:33 - 3:34 number
            |										integer
            |											2
            |							4:25 - 4:26 number
            |								integer
            |									3
            |					5:17 - 5:18 number
            |						integer
            |							4
            |			6:9 - 6:10 number
            |				integer
            |					5
            |],
      testCaseParse
        "nesting11 -> \n\t1\n\t\t\t2\n\t\t3"
        [sbt|
            |1:1 - 4:18 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting11
            |	1:11 - 4:18 function body
            |		2:9 - 4:18 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			3:25 - 4:18 nested expressions
            |				3:25 - 4:18 expressions block
            |					3:25 - 3:26 nested expressions
            |						3:25 - 3:26 expressions block
            |							3:25 - 3:26 number
            |								integer
            |									2
            |					4:17 - 4:18 number
            |						integer
            |							3
            |],
      testCaseParse
        "nesting12 -> \n\t1(2)3"
        [sbt|
            |1:1 - 2:14 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting12
            |	1:11 - 2:14 function body
            |		2:9 - 2:14 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			2:10 - 2:13 higher priority expressions
            |				2:11 - 2:12 expressions block
            |					2:11 - 2:12 number
            |						integer
            |							2
            |			2:13 - 2:14 number
            |				integer
            |					3
            |],
      testCaseParse
        "nesting13 -> \n\t1\n\t(2)3"
        [sbt|
            |1:1 - 3:13 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting13
            |	1:11 - 3:13 function body
            |		2:9 - 2:10 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |		3:9 - 3:13 expressions block
            |			3:9 - 3:12 higher priority expressions
            |				3:10 - 3:11 expressions block
            |					3:10 - 3:11 number
            |						integer
            |							2
            |			3:12 - 3:13 number
            |				integer
            |					3
            |],
      testCaseParse
        "nesting14 -> \n\t1(\n\t2)3"
        [sbt|
            |1:1 - 3:12 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting14
            |	1:11 - 3:12 function body
            |		2:9 - 3:12 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			2:10 - 3:11 higher priority expressions
            |				3:9 - 3:10 expressions block
            |					3:9 - 3:10 number
            |						integer
            |							2
            |			3:11 - 3:12 number
            |				integer
            |					3
            |],
      testCaseParse
        "nesting15 -> \n\t1(2\n\t)3"
        [sbt|
            |1:1 - 3:11 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting15
            |	1:11 - 3:11 function body
            |		2:9 - 3:11 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			2:10 - 3:10 higher priority expressions
            |				2:11 - 2:12 expressions block
            |					2:11 - 2:12 number
            |						integer
            |							2
            |			3:10 - 3:11 number
            |				integer
            |					3
            |],
      testCaseParse
        "nesting16 -> \n\t1(2)\n\t3"
        [sbt|
            |1:1 - 3:10 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting16
            |	1:11 - 3:10 function body
            |		2:9 - 2:13 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			2:10 - 2:13 higher priority expressions
            |				2:11 - 2:12 expressions block
            |					2:11 - 2:12 number
            |						integer
            |							2
            |		3:9 - 3:10 expressions block
            |			3:9 - 3:10 number
            |				integer
            |					3
            |],
      testCaseParse
        "nesting17 -> \n\t1\n\t(2)\n\t3"
        [sbt|
            |1:1 - 4:10 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting17
            |	1:11 - 4:10 function body
            |		2:9 - 2:10 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |		3:9 - 3:12 expressions block
            |			3:9 - 3:12 higher priority expressions
            |				3:10 - 3:11 expressions block
            |					3:10 - 3:11 number
            |						integer
            |							2
            |		4:9 - 4:10 expressions block
            |			4:9 - 4:10 number
            |				integer
            |					3
            |],
      testCaseParse
        "nesting18 -> \n\t1\n\t(\n\t2)\n\t3"
        [sbt|
            |1:1 - 5:10 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting18
            |	1:11 - 5:10 function body
            |		2:9 - 2:10 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |		3:9 - 4:11 expressions block
            |			3:9 - 4:11 higher priority expressions
            |				4:9 - 4:10 expressions block
            |					4:9 - 4:10 number
            |						integer
            |							2
            |		5:9 - 5:10 expressions block
            |			5:9 - 5:10 number
            |				integer
            |					3
            |],
      testCaseParse
        "nesting19 -> \n\t1\n\t(\n\t2\n\t)\n\t3"
        [sbt|
            |1:1 - 6:10 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting19
            |	1:11 - 6:10 function body
            |		2:9 - 2:10 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |		3:9 - 5:10 expressions block
            |			3:9 - 5:10 higher priority expressions
            |				4:9 - 4:10 expressions block
            |					4:9 - 4:10 number
            |						integer
            |							2
            |		6:9 - 6:10 expressions block
            |			6:9 - 6:10 number
            |				integer
            |					3
            |],
      testCaseParse
        "nesting20 -> \n\t1(2\n\t3)\n\t4"
        [sbt|
            |1:1 - 4:10 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting20
            |	1:11 - 4:10 function body
            |		2:9 - 3:11 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			2:10 - 3:11 higher priority expressions
            |				2:11 - 3:10 expressions block
            |					2:11 - 2:12 number
            |						integer
            |							2
            |					3:9 - 3:10 nested expressions
            |						3:9 - 3:10 expressions block
            |							3:9 - 3:10 number
            |								integer
            |									3
            |		4:9 - 4:10 expressions block
            |			4:9 - 4:10 number
            |				integer
            |					4
            |],
      testCaseParse
        "nesting21 -> \n\t1(2\n\t\t3\n\t4)\n\t5"
        [sbt|
            |1:1 - 5:10 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting21
            |	1:11 - 5:10 function body
            |		2:9 - 4:11 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			2:10 - 4:11 higher priority expressions
            |				2:11 - 4:10 expressions block
            |					2:11 - 2:12 number
            |						integer
            |							2
            |					3:17 - 4:10 nested expressions
            |						3:17 - 4:10 expressions block
            |							3:17 - 3:18 nested expressions
            |								3:17 - 3:18 expressions block
            |									3:17 - 3:18 number
            |										integer
            |											3
            |							4:9 - 4:10 number
            |								integer
            |									4
            |		5:9 - 5:10 expressions block
            |			5:9 - 5:10 number
            |				integer
            |					5
            |],
      testCaseParse
        "nesting22 -> \n\t1(\n\t\t2\n\t3\n\t4)\n\t5"
        [sbt|
            |1:1 - 6:10 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting22
            |	1:11 - 6:10 function body
            |		2:9 - 5:11 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			2:10 - 5:11 higher priority expressions
            |				3:17 - 4:10 expressions block
            |					3:17 - 3:18 nested expressions
            |						3:17 - 3:18 expressions block
            |							3:17 - 3:18 number
            |								integer
            |									2
            |					4:9 - 4:10 number
            |						integer
            |							3
            |				5:9 - 5:10 expressions block
            |					5:9 - 5:10 number
            |						integer
            |							4
            |		6:9 - 6:10 expressions block
            |			6:9 - 6:10 number
            |				integer
            |					5
            |],
      testCaseParse
        "nesting23 -> \n\t1(\n\t2\n\t3\n\t\t4)\n\t5"
        [sbt|
            |1:1 - 6:10 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting23
            |	1:11 - 6:10 function body
            |		2:9 - 5:19 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			2:10 - 5:19 higher priority expressions
            |				3:9 - 3:10 expressions block
            |					3:9 - 3:10 number
            |						integer
            |							2
            |				4:9 - 5:18 expressions block
            |					4:9 - 4:10 number
            |						integer
            |							3
            |					5:17 - 5:18 nested expressions
            |						5:17 - 5:18 expressions block
            |							5:17 - 5:18 number
            |								integer
            |									4
            |		6:9 - 6:10 expressions block
            |			6:9 - 6:10 number
            |				integer
            |					5
            |],
      testCaseParse
        "nesting24 -> \n\t1\n\t\t(\n\t\t2\n\t\t3\n\t\t4)\n\t5"
        [sbt|
            |1:1 - 7:10 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting24
            |	1:11 - 7:10 function body
            |		2:9 - 6:19 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			3:17 - 6:19 nested expressions
            |				3:17 - 6:19 expressions block
            |					3:17 - 6:19 higher priority expressions
            |						4:17 - 4:18 expressions block
            |							4:17 - 4:18 number
            |								integer
            |									2
            |						5:17 - 5:18 expressions block
            |							5:17 - 5:18 number
            |								integer
            |									3
            |						6:17 - 6:18 expressions block
            |							6:17 - 6:18 number
            |								integer
            |									4
            |		7:9 - 7:10 expressions block
            |			7:9 - 7:10 number
            |				integer
            |					5
            |],
      testCaseParse
        "nesting25 -> \n\t\t(\n\t\t1\n\t\t2\n\t\t3)\n\t4\n\t5"
        [sbt|
            |1:1 - 7:10 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting25
            |	1:11 - 7:10 function body
            |		2:17 - 6:10 expressions block
            |			2:17 - 5:19 nested expressions
            |				2:17 - 5:19 expressions block
            |					2:17 - 5:19 higher priority expressions
            |						3:17 - 3:18 expressions block
            |							3:17 - 3:18 number
            |								integer
            |									1
            |						4:17 - 4:18 expressions block
            |							4:17 - 4:18 number
            |								integer
            |									2
            |						5:17 - 5:18 expressions block
            |							5:17 - 5:18 number
            |								integer
            |									3
            |			6:9 - 6:10 number
            |				integer
            |					4
            |		7:9 - 7:10 expressions block
            |			7:9 - 7:10 number
            |				integer
            |					5
            |],
      testCaseParse
        "nesting26 -> \n\t1\n\t2(\n\t\t3\n\t\t4\n\t\t5)"
        [sbt|
            |1:1 - 6:19 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting26
            |	1:11 - 6:19 function body
            |		2:9 - 2:10 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |		3:9 - 6:19 expressions block
            |			3:9 - 3:10 number
            |				integer
            |					2
            |			3:10 - 6:19 higher priority expressions
            |				4:17 - 4:18 expressions block
            |					4:17 - 4:18 number
            |						integer
            |							3
            |				5:17 - 5:18 expressions block
            |					5:17 - 5:18 number
            |						integer
            |							4
            |				6:17 - 6:18 expressions block
            |					6:17 - 6:18 number
            |						integer
            |							5
            |],
      testCaseParse
        "nesting27 -> \n\t1(\n\t\t2 3 4\n\t\t5 6 7\n\t\t8 9 10)\n\t11"
        [sbt|
            |1:1 - 6:11 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting27
            |	1:11 - 6:11 function body
            |		2:9 - 5:24 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			2:10 - 5:24 higher priority expressions
            |				3:17 - 3:22 expressions block
            |					3:17 - 3:18 number
            |						integer
            |							2
            |					3:19 - 3:20 number
            |						integer
            |							3
            |					3:21 - 3:22 number
            |						integer
            |							4
            |				4:17 - 4:22 expressions block
            |					4:17 - 4:18 number
            |						integer
            |							5
            |					4:19 - 4:20 number
            |						integer
            |							6
            |					4:21 - 4:22 number
            |						integer
            |							7
            |				5:17 - 5:23 expressions block
            |					5:17 - 5:18 number
            |						integer
            |							8
            |					5:19 - 5:20 number
            |						integer
            |							9
            |					5:21 - 5:23 number
            |						integer
            |							10
            |		6:9 - 6:11 expressions block
            |			6:9 - 6:11 number
            |				integer
            |					11
            |],
      testCaseParse
        "nesting28 -> \n\t1 2 3(\n\t\t3 4.8e7 5\n\t\t)\n\t6 7 8"
        [sbt|
            |1:1 - 5:14 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting28
            |	1:11 - 5:14 function body
            |		2:9 - 4:18 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			2:11 - 2:12 number
            |				integer
            |					2
            |			2:13 - 2:14 number
            |				integer
            |					3
            |			2:14 - 4:18 higher priority expressions
            |				3:17 - 3:26 expressions block
            |					3:17 - 3:18 number
            |						integer
            |							3
            |					3:19 - 3:24 number
            |						integer
            |							4
            |						decimal
            |							8
            |						exponent
            |							7
            |					3:25 - 3:26 number
            |						integer
            |							5
            |		5:9 - 5:14 expressions block
            |			5:9 - 5:10 number
            |				integer
            |					6
            |			5:11 - 5:12 number
            |				integer
            |					7
            |			5:13 - 5:14 number
            |				integer
            |					8
            |],
      testCaseParse
        "nesting29 -> \n\t1 2 3\n\t(\n\t\t3 4 5\n\t\t)\n\t6 7 8"
        [sbt|
            |1:1 - 6:14 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting29
            |	1:11 - 6:14 function body
            |		2:9 - 2:14 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			2:11 - 2:12 number
            |				integer
            |					2
            |			2:13 - 2:14 number
            |				integer
            |					3
            |		3:9 - 5:18 expressions block
            |			3:9 - 5:18 higher priority expressions
            |				4:17 - 4:22 expressions block
            |					4:17 - 4:18 number
            |						integer
            |							3
            |					4:19 - 4:20 number
            |						integer
            |							4
            |					4:21 - 4:22 number
            |						integer
            |							5
            |		6:9 - 6:14 expressions block
            |			6:9 - 6:10 number
            |				integer
            |					6
            |			6:11 - 6:12 number
            |				integer
            |					7
            |			6:13 - 6:14 number
            |				integer
            |					8
            |],
      testCaseParse
        "nesting30 ->\n\t\t1\n\t(2)"
        [sbt|
            |1:1 - 3:12 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting30
            |	1:11 - 3:12 function body
            |		2:17 - 3:12 expressions block
            |			2:17 - 2:18 nested expressions
            |				2:17 - 2:18 expressions block
            |					2:17 - 2:18 number
            |						integer
            |							1
            |			3:9 - 3:12 higher priority expressions
            |				3:10 - 3:11 expressions block
            |					3:10 - 3:11 number
            |						integer
            |							2
            |],
      testCaseParse
        "nesting31 ->\n\t\t1\n\t\t\t2.3\n\t\t\t(\t\t\t3)\n\t4"
        [sbt|
            |1:1 - 5:10 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting31
            |	1:11 - 5:10 function body
            |		2:17 - 5:10 expressions block
            |			2:17 - 4:51 nested expressions
            |				2:17 - 4:51 expressions block
            |					2:17 - 2:18 number
            |						integer
            |							1
            |					3:25 - 4:51 nested expressions
            |						3:25 - 3:28 expressions block
            |							3:25 - 3:28 number
            |								integer
            |									2
            |								decimal
            |									3
            |						4:25 - 4:51 expressions block
            |							4:25 - 4:51 higher priority expressions
            |								4:49 - 4:50 expressions block
            |									4:49 - 4:50 number
            |										integer
            |											3
            |			5:9 - 5:10 number
            |				integer
            |					4
            |],
      testCaseParse
        "nesting32 ->\n\t1 \t\t\t(\n\t\t\t(\n\t\t\t4e6\n \t\t\t\t5))"
        [sbt|
            |1:1 - 5:36 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting32
            |	1:11 - 5:36 function body
            |		2:9 - 5:36 expressions block
            |			2:9 - 2:10 number
            |				integer
            |					1
            |			2:33 - 5:36 higher priority expressions
            |				3:25 - 5:35 expressions block
            |					3:25 - 5:35 higher priority expressions
            |						4:25 - 5:34 expressions block
            |							4:25 - 4:28 number
            |								integer
            |									4
            |								exponent
            |									6
            |							5:33 - 5:34 nested expressions
            |								5:33 - 5:34 expressions block
            |									5:33 - 5:34 number
            |										integer
            |											5
            |],
      testCaseParse
        "nesting33 -> 1\n\t2\n\t3"
        [sbt|
            |1:1 - 3:10 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting33
            |	1:11 - 3:10 function body
            |		1:14 - 3:10 expressions block
            |			1:14 - 1:15 number
            |				integer
            |					1
            |			2:9 - 3:10 nested expressions
            |				2:9 - 2:10 expressions block
            |					2:9 - 2:10 number
            |						integer
            |							2
            |				3:9 - 3:10 expressions block
            |					3:9 - 3:10 number
            |						integer
            |							3
            |],
      testCaseParse
        "nesting34 -> 1\n\t2\n\t\t3"
        [sbt|
            |1:1 - 3:18 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting34
            |	1:11 - 3:18 function body
            |		1:14 - 3:18 expressions block
            |			1:14 - 1:15 number
            |				integer
            |					1
            |			2:9 - 3:18 nested expressions
            |				2:9 - 3:18 expressions block
            |					2:9 - 2:10 number
            |						integer
            |							2
            |					3:17 - 3:18 nested expressions
            |						3:17 - 3:18 expressions block
            |							3:17 - 3:18 number
            |								integer
            |									3
            |],
      testCaseParse
        "nesting35\n\targ-> 1 2\n\t\t3"
        [sbt|
            |1:1 - 3:18 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting35
            |		2:9 - 2:12 parameters
            |			2:9 - 2:12 parameter 1
            |				2:9 - 2:12 alphnumeric identifier
            |					arg
            |	2:12 - 3:18 function body
            |		2:15 - 3:18 expressions block
            |			2:15 - 2:16 number
            |				integer
            |					1
            |			2:17 - 2:18 number
            |				integer
            |					2
            |			3:17 - 3:18 nested expressions
            |				3:17 - 3:18 expressions block
            |					3:17 - 3:18 number
            |						integer
            |							3
            |],
      testCaseParse
        "nesting36\n\targ->\n\t(1\n\t)\t2"
        [sbt|
            |1:1 - 4:18 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting36
            |		2:9 - 2:12 parameters
            |			2:9 - 2:12 parameter 1
            |				2:9 - 2:12 alphnumeric identifier
            |					arg
            |	2:12 - 4:18 function body
            |		3:9 - 4:18 expressions block
            |			3:9 - 4:10 higher priority expressions
            |				3:10 - 3:11 expressions block
            |					3:10 - 3:11 number
            |						integer
            |							1
            |			4:17 - 4:18 number
            |				integer
            |					2
            |],
      testCaseParse
        "nesting37\n\targ->\n\t(\n\t1\n\t\t2\n\t\t3)\t4"
        [sbt|
            |1:1 - 6:26 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting37
            |		2:9 - 2:12 parameters
            |			2:9 - 2:12 parameter 1
            |				2:9 - 2:12 alphnumeric identifier
            |					arg
            |	2:12 - 6:26 function body
            |		3:9 - 6:26 expressions block
            |			3:9 - 6:19 higher priority expressions
            |				4:9 - 6:18 expressions block
            |					4:9 - 4:10 number
            |						integer
            |							1
            |					5:17 - 6:18 nested expressions
            |						5:17 - 5:18 expressions block
            |							5:17 - 5:18 number
            |								integer
            |									2
            |						6:17 - 6:18 expressions block
            |							6:17 - 6:18 number
            |								integer
            |									3
            |			6:25 - 6:26 number
            |				integer
            |					4
            |],
      testCaseParse
        "nesting38 -> \n\t\t1\n\t2\n\t3"
        [sbt|
            |1:1 - 4:10 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting38
            |	1:11 - 4:10 function body
            |		2:17 - 3:10 expressions block
            |			2:17 - 2:18 nested expressions
            |				2:17 - 2:18 expressions block
            |					2:17 - 2:18 number
            |						integer
            |							1
            |			3:9 - 3:10 number
            |				integer
            |					2
            |		4:9 - 4:10 expressions block
            |			4:9 - 4:10 number
            |				integer
            |					3
            |],
      testCaseParse
        "nesting39 ->\n\t(a\n\t)\t\t(b)\n\t\tc"
        [sbt|
          |1:1 - 4:18 function
          |	1:1 - 1:10 prefix function signature
          |		1:1 - 1:10 name
          |			1:1 - 1:10 alphnumeric identifier
          |				nesting39
          |	1:11 - 4:18 function body
          |		2:9 - 4:18 expressions block
          |			2:9 - 3:10 higher priority expressions
          |				2:10 - 2:11 expressions block
          |					2:10 - 2:11 alphnumeric identifier
          |						a
          |			3:25 - 3:28 higher priority expressions
          |				3:26 - 3:27 expressions block
          |					3:26 - 3:27 alphnumeric identifier
          |						b
          |			4:17 - 4:18 nested expressions
          |				4:17 - 4:18 expressions block
          |					4:17 - 4:18 alphnumeric identifier
          |						c
          |],
      testCaseParse
        "nesting40->\n\t   a\n\t   >\n\tb\n\n\n\t(\n\tc\n\t)"
        [sbt|
          |1:1 - 9:10 function
          |	1:1 - 1:10 prefix function signature
          |		1:1 - 1:10 name
          |			1:1 - 1:10 alphnumeric identifier
          |				nesting40
          |	1:10 - 9:10 function body
          |		2:12 - 4:10 expressions block
          |			2:12 - 3:13 nested expressions
          |				2:12 - 2:13 expressions block
          |					2:12 - 2:13 alphnumeric identifier
          |						a
          |				3:12 - 3:13 expressions block
          |					3:12 - 3:13 special identifier
          |						>
          |			4:9 - 4:10 alphnumeric identifier
          |				b
          |		7:9 - 9:10 expressions block
          |			7:9 - 9:10 higher priority expressions
          |				8:9 - 8:10 expressions block
          |					8:9 - 8:10 alphnumeric identifier
          |						c
          |],
      testCaseParse
        "nesting41 -> \n\t\t1\n\t2\n\t\t3"
        [sbt|
            |1:1 - 4:18 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				nesting41
            |	1:11 - 4:18 function body
            |		2:17 - 4:18 expressions block
            |			2:17 - 2:18 nested expressions
            |				2:17 - 2:18 expressions block
            |					2:17 - 2:18 number
            |						integer
            |							1
            |			3:9 - 3:10 number
            |				integer
            |					2
            |			4:17 - 4:18 nested expressions
            |				4:17 - 4:18 expressions block
            |					4:17 - 4:18 number
            |						integer
            |							3
            |],
      testCaseParse
        "function\n\targ1\n\targ2 -> arg1"
        [sbt|
            |1:1 - 3:21 function
            |	1:1 - 1:9 prefix function signature
            |		1:1 - 1:9 name
            |			1:1 - 1:9 alphnumeric identifier
            |				function
            |		2:9 - 3:13 parameters
            |			2:9 - 2:13 parameter 1
            |				2:9 - 2:13 alphnumeric identifier
            |					arg1
            |			3:9 - 3:13 parameter 2
            |				3:9 - 3:13 alphnumeric identifier
            |					arg2
            |	3:14 - 3:21 function body
            |		3:17 - 3:21 expressions block
            |			3:17 - 3:21 alphnumeric identifier
            |				arg1
            |],
      testCaseParse
        "(x)div(y)->x"
        [sbt|
            |1:1 - 1:13 function
            |	1:4 - 1:7 infix function signature
            |		1:4 - 1:7 name
            |			1:4 - 1:7 alphnumeric identifier
            |				div
            |		1:1 - 1:10 parameters
            |			1:1 - 1:4 left parameter 
            |				1:1 - 1:4 higher priority parameter
            |					first order parameter
            |						1:2 - 1:3 alphnumeric identifier
            |							x
            |			1:7 - 1:10 right parameter 
            |				1:7 - 1:10 higher priority parameter
            |					first order parameter
            |						1:8 - 1:9 alphnumeric identifier
            |							y
            |	1:10 - 1:13 function body
            |		1:12 - 1:13 expressions block
            |			1:12 - 1:13 alphnumeric identifier
            |				x
            |],
      testCaseParse
        "x*-%:::-:xy->x"
        [sbt|
            |1:1 - 1:15 function
            |	1:2 - 1:10 infix function signature
            |		1:2 - 1:10 name
            |			1:2 - 1:10 special identifier
            |				*-%:::-:
            |		1:1 - 1:12 parameters
            |			1:1 - 1:2 left parameter 
            |				1:1 - 1:2 alphnumeric identifier
            |					x
            |			1:10 - 1:12 right parameter 
            |				1:10 - 1:12 alphnumeric identifier
            |					xy
            |	1:12 - 1:15 function body
            |		1:14 - 1:15 expressions block
            |			1:14 - 1:15 alphnumeric identifier
            |				x
            |],
      testCaseParse
        "number ->  1e-1 -1e1 0e1"
        [sbt|
            |1:1 - 1:25 function
            |	1:1 - 1:7 prefix function signature
            |		1:1 - 1:7 name
            |			1:1 - 1:7 alphnumeric identifier
            |				number
            |	1:8 - 1:25 function body
            |		1:12 - 1:25 expressions block
            |			1:12 - 1:16 number
            |				integer
            |					1
            |				exponent
            |					-1
            |			1:17 - 1:21 number
            |				integer
            |					1
            |				exponent
            |					1
            |				signed
            |			1:22 - 1:25 number
            |				integer
            |					0
            |				exponent
            |					1
            |],
      testCaseParse
        "some_func ->  \"beginning \"\"then some text inside\"\" and  \"\"\"\"again\"\"\"\"  and then continuation\" "
        [sbt|
            |1:1 - 1:94 function
            |	1:1 - 1:10 prefix function signature
            |		1:1 - 1:10 name
            |			1:1 - 1:10 alphnumeric identifier
            |				some_func
            |	1:11 - 1:94 function body
            |		1:15 - 1:94 expressions block
            |			1:15 - 1:94 text
            |				beginning "then some text inside" and  ""again""  and then continuation
            |],
      testCaseParse
        "three_body_lines_func ->\n\ta\n\tb\n\tc"
        [sbt|
            |1:1 - 4:10 function
            |	1:1 - 1:22 prefix function signature
            |		1:1 - 1:22 name
            |			1:1 - 1:22 alphnumeric identifier
            |				three_body_lines_func
            |	1:23 - 4:10 function body
            |		2:9 - 2:10 expressions block
            |			2:9 - 2:10 alphnumeric identifier
            |				a
            |		3:9 - 3:10 expressions block
            |			3:9 - 3:10 alphnumeric identifier
            |				b
            |		4:9 - 4:10 expressions block
            |			4:9 - 4:10 alphnumeric identifier
            |				c
            |],
      testCaseParse
        "func_with_higher_order_func (higher_order_func _ _ _ ) -> 1"
        [sbt|
            |1:1 - 1:60 function
            |	1:1 - 1:28 prefix function signature
            |		1:1 - 1:28 name
            |			1:1 - 1:28 alphnumeric identifier
            |				func_with_higher_order_func
            |		1:29 - 1:55 parameters
            |			1:29 - 1:55 parameter 1
            |				1:29 - 1:55 higher priority parameter
            |					higher order prefix parameter
            |							1:30 - 1:47 alphnumeric identifier
            |								higher_order_func
            |							params count
            |								3
            |	1:56 - 1:60 function body
            |		1:59 - 1:60 expressions block
            |			1:59 - 1:60 number
            |				integer
            |					1
            |],
      testCaseParse
        "func_with_higher_order_spec_func (! _  ) -> 1"
        [sbt|
            |1:1 - 1:46 function
            |	1:1 - 1:33 prefix function signature
            |		1:1 - 1:33 name
            |			1:1 - 1:33 alphnumeric identifier
            |				func_with_higher_order_spec_func
            |		1:34 - 1:41 parameters
            |			1:34 - 1:41 parameter 1
            |				1:34 - 1:41 higher priority parameter
            |					higher order prefix parameter
            |							1:35 - 1:36 special identifier
            |								!
            |							params count
            |								1
            |	1:42 - 1:46 function body
            |		1:45 - 1:46 expressions block
            |			1:45 - 1:46 number
            |				integer
            |					1
            |],
      testCaseParse
        "func_with_infix_higher_order_func ( _ higher_order_func _ ) -> 1"
        [sbt|
            |1:1 - 1:65 function
            |	1:1 - 1:34 prefix function signature
            |		1:1 - 1:34 name
            |			1:1 - 1:34 alphnumeric identifier
            |				func_with_infix_higher_order_func
            |		1:35 - 1:60 parameters
            |			1:35 - 1:60 parameter 1
            |				1:35 - 1:60 higher priority parameter
            |					higher order infix parameter
            |						1:39 - 1:56 alphnumeric identifier
            |							higher_order_func
            |	1:61 - 1:65 function body
            |		1:64 - 1:65 expressions block
            |			1:64 - 1:65 number
            |				integer
            |					1
            |],
      testCaseParse
        "func_with_infix_spec_higher_order_func ( _ + _ ) -> 1"
        [sbt|
            |1:1 - 1:54 function
            |	1:1 - 1:39 prefix function signature
            |		1:1 - 1:39 name
            |			1:1 - 1:39 alphnumeric identifier
            |				func_with_infix_spec_higher_order_func
            |		1:40 - 1:49 parameters
            |			1:40 - 1:49 parameter 1
            |				1:40 - 1:49 higher priority parameter
            |					higher order infix parameter
            |						1:44 - 1:45 special identifier
            |							+
            |	1:50 - 1:54 function body
            |		1:53 - 1:54 expressions block
            |			1:53 - 1:54 number
            |				integer
            |					1
            |],
      testCaseParse
        "! x -> x"
        [sbt|
            |1:1 - 1:9 function
            |	1:1 - 1:2 prefix function signature
            |		1:1 - 1:2 name
            |			1:1 - 1:2 special identifier
            |				!
            |		1:3 - 1:4 parameters
            |			1:3 - 1:4 parameter 1
            |				1:3 - 1:4 alphnumeric identifier
            |					x
            |	1:5 - 1:9 function body
            |		1:8 - 1:9 expressions block
            |			1:8 - 1:9 alphnumeric identifier
            |				x
            |],
      testCaseParse
        "f -> \n\taddress {\n\t\t\ts<- street\n\t\t}<-  value"
        [sbt|
            |1:1 - 4:27 function
            |	1:1 - 1:2 prefix function signature
            |		1:1 - 1:2 name
            |			1:1 - 1:2 alphnumeric identifier
            |				f
            |	1:3 - 4:27 function body
            |		2:9 - 4:27 expressions block
            |			2:9 - 2:16 alphnumeric identifier
            |				address
            |			2:17 - 4:18 higher priority expressions
            |				3:25 - 3:35 expressions block
            |					3:25 - 3:26 alphnumeric identifier
            |						s
            |					3:26 - 3:28 special identifier
            |						<-
            |					3:29 - 3:35 alphnumeric identifier
            |						street
            |			4:18 - 4:20 special identifier
            |				<-
            |			4:22 - 4:27 alphnumeric identifier
            |				value
            |]
    ]
