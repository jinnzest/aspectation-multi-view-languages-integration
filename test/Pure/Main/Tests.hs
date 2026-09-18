module Pure.Main.Tests
  ( mainTests,
  )
where

import Pure.Main.Syntax.Lexer.LexerSourceTests (lexerFormattedWritingTests)
import Pure.Main.Syntax.Lexer.Parsing.LexerErrorTests (lexerErrorTests)
import Pure.Main.Syntax.Lexer.Parsing.LexerGeneratorTest (lexerGeneratorTest)
import Pure.Main.Syntax.Lexer.Parsing.LexerTests (lexerTests)
import Pure.Main.Syntax.Lexer.Writing.LexerRangedTests (lexerRangedWritingTests)
import Test.Tasty (TestTree, testGroup)

mainTests :: TestTree
mainTests =
  testGroup
    "Main pure tests"
    [ lexerRangedWritingTests,
      lexerTests,
      lexerFormattedWritingTests,
      lexerErrorTests,
      lexerGeneratorTest
    ]
