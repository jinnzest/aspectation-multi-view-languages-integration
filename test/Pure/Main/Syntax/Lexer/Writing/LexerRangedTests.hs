module Pure.Main.Syntax.Lexer.Writing.LexerRangedTests
  ( lexerRangedWritingTests,
  )
where

import Data.Bool (Bool (False))
import Data.Function (($))
import Data.List.NonEmpty as NE (fromList)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Text (Text, unpack)
import Main.Syntax.Lexer.LexerModel
  ( BodyExpr
      ( AlphaNumBodyExpr,
        HigherPriorityExpr,
        NumberBodyExpr,
        TextBodyExpr
      ),
    ExprsBlock (ExprsBlock, ebExprs, ebSemicolon),
    Func (Func, fBody, fSig),
    FuncBody (FuncBody),
    FuncSig (FuncSig, fsName, fsParams, fsURL),
    FuncSigParams (PrefixSigParams),
    NameSigExpr (AlphaNumNameSigExpr),
    Nested (InBracketsNested, IndentedNested, ibExprs, ibKind),
    Number (Number, dec, exp, mant, sign),
    ParamSigExpr (AlphaNumParamSigExpr),
  )
import Main.Syntax.Lexer.Writing.Ranged (writeFuncs)
import Pure.Main.Syntax.Shared (mkOcRanged, mkRanged)
import Shared.Location.Data
  ( BracketsKind (RoundBrackets),
    Ranged,
  )
import Shared.Text.Utils (nL, withBorder)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Text.Shakespeare.Text (sbt, st)

alignAR :: [Ranged Func] -> Text
alignAR other = [st|#{writeFuncs other}#{nL}|]

testCaseParse :: Text -> [Ranged Func] -> Text -> TestTree
testCaseParse name source expected = testCase (unpack $ withBorder name) $ assertEqual "" expected (alignAR source)

lexerRangedWritingTests :: TestTree
lexerRangedWritingTests =
  testGroup
    "Syntax ranged writer tests"
    [ testCaseParse
        "write a function without args which returns an identifier"
        [ mkRanged 1 2 7 8 $
            Func
              { fSig = FuncSig {fsName = AlphaNumNameSigExpr $ mkRanged 1 2 3 4 "Name", fsParams = PrefixSigParams [], fsURL = ""},
                fBody = FuncBody $ mkRanged 5 6 7 8 $ NE.fromList [ExprsBlock {ebExprs = NE.fromList [AlphaNumBodyExpr $ mkRanged 5 6 7 8 "id"], ebSemicolon = Nothing}]
              }
        ]
        [sbt|
            |1:2 - 7:8 function
            |	1:2 - 3:4 prefix function signature
            |		1:2 - 3:4 name
            |			1:2 - 3:4 alphnumeric identifier
            |				Name
            |	5:6 - 7:8 function body
            |		5:6 - 7:8 expressions block
            |			5:6 - 7:8 alphnumeric identifier
            |				id
            |],
      testCaseParse
        "write a function which take an parameter and returns an alpha num identifier number"
        [ mkRanged 1 2 11 12 $
            Func
              { fSig =
                  FuncSig
                    { fsName = AlphaNumNameSigExpr $ mkRanged 1 2 3 4 "Name",
                      fsParams = PrefixSigParams [AlphaNumParamSigExpr $ mkRanged 5 6 7 8 "Param"],
                      fsURL = ""
                    },
                fBody = FuncBody $ mkRanged 13 14 15 16 $ NE.fromList [ExprsBlock {ebExprs = NE.fromList [AlphaNumBodyExpr $ mkRanged 9 10 11 12 "1"], ebSemicolon = Nothing}]
              }
        ]
        [sbt|
            |1:2 - 11:12 function
            |	1:2 - 3:4 prefix function signature
            |		1:2 - 3:4 name
            |			1:2 - 3:4 alphnumeric identifier
            |				Name
            |		5:6 - 7:8 parameters
            |			5:6 - 7:8 parameter 1
            |				5:6 - 7:8 alphnumeric identifier
            |					Param
            |	13:14 - 15:16 function body
            |		9:10 - 11:12 expressions block
            |			9:10 - 11:12 alphnumeric identifier
            |				1
            |],
      testCaseParse
        "write a function two parameters that returns text"
        [ mkRanged 1 2 23 24 $
            Func
              { fSig =
                  FuncSig
                    { fsName = AlphaNumNameSigExpr $ mkRanged 1 2 3 4 "Name",
                      fsParams =
                        PrefixSigParams
                          [ AlphaNumParamSigExpr $ mkRanged 9 10 11 12 "Param1",
                            AlphaNumParamSigExpr $ mkRanged 13 14 15 16 "Param2"
                          ],
                      fsURL = ""
                    },
                fBody = FuncBody $ mkRanged 25 26 27 28 $ NE.fromList [ExprsBlock {ebExprs = NE.fromList [TextBodyExpr $ mkRanged 21 22 23 24 "text"], ebSemicolon = Nothing}]
              }
        ]
        [sbt|
            |1:2 - 23:24 function
            |	1:2 - 3:4 prefix function signature
            |		1:2 - 3:4 name
            |			1:2 - 3:4 alphnumeric identifier
            |				Name
            |		9:10 - 15:16 parameters
            |			9:10 - 11:12 parameter 1
            |				9:10 - 11:12 alphnumeric identifier
            |					Param1
            |			13:14 - 15:16 parameter 2
            |				13:14 - 15:16 alphnumeric identifier
            |					Param2
            |	25:26 - 27:28 function body
            |		21:22 - 23:24 expressions block
            |			21:22 - 23:24 text
            |				text
            |],
      testCaseParse
        "write a function with two parameters having a multi-lined body of different expression types"
        [ mkRanged 1 2 27 28 $
            Func
              { fSig =
                  FuncSig
                    { fsName = AlphaNumNameSigExpr $ mkRanged 1 2 3 4 "Name",
                      fsParams =
                        PrefixSigParams
                          [ AlphaNumParamSigExpr $ mkRanged 5 6 7 8 "Param1",
                            AlphaNumParamSigExpr $ mkRanged 9 10 11 12 "Param2"
                          ],
                      fsURL = ""
                    },
                fBody =
                  FuncBody
                    $ mkRanged
                      29
                      30
                      31
                      32
                    $ NE.fromList
                      [ ExprsBlock
                          { ebExprs =
                              NE.fromList
                                [ AlphaNumBodyExpr $ mkRanged 13 14 15 16 "id",
                                  AlphaNumBodyExpr $ mkRanged 17 18 19 20 "1st",
                                  NumberBodyExpr $ mkRanged 21 22 23 24 $ Number {mant = 1, dec = Just 2, exp = Nothing, sign = False},
                                  TextBodyExpr $ mkRanged 25 26 27 28 "the\tfirst line of text\nthe second line of text"
                                ],
                            ebSemicolon = Nothing
                          }
                      ]
              }
        ]
        [sbt|
            |1:2 - 27:28 function
            |	1:2 - 3:4 prefix function signature
            |		1:2 - 3:4 name
            |			1:2 - 3:4 alphnumeric identifier
            |				Name
            |		5:6 - 11:12 parameters
            |			5:6 - 7:8 parameter 1
            |				5:6 - 7:8 alphnumeric identifier
            |					Param1
            |			9:10 - 11:12 parameter 2
            |				9:10 - 11:12 alphnumeric identifier
            |					Param2
            |	29:30 - 31:32 function body
            |		13:14 - 27:28 expressions block
            |			13:14 - 15:16 alphnumeric identifier
            |				id
            |			17:18 - 19:20 alphnumeric identifier
            |				1st
            |			21:22 - 23:24 number
            |				integer
            |					1
            |				decimal
            |					2
            |			25:26 - 27:28 text
            |				the	first line of text
            |the second line of text
            |],
      testCaseParse
        "write a function without args which contains nested expressions"
        [ mkRanged 1 2 23 24 $
            Func
              { fSig = FuncSig {fsName = AlphaNumNameSigExpr $ mkRanged 1 2 3 4 "Name", fsParams = PrefixSigParams [], fsURL = ""},
                fBody =
                  FuncBody
                    $ mkRanged
                      22
                      23
                      24
                      25
                    $ NE.fromList
                      [ ExprsBlock
                          { ebExprs =
                              NE.fromList
                                [ AlphaNumBodyExpr $ mkRanged 5 6 7 8 "id",
                                  HigherPriorityExpr
                                    ( IndentedNested $
                                        NE.fromList
                                          [ ExprsBlock
                                              { ebExprs =
                                                  NE.fromList
                                                    [ AlphaNumBodyExpr $ mkRanged 13 14 15 16 "subId",
                                                      AlphaNumBodyExpr $ mkRanged 17 18 19 20 "subId2"
                                                    ],
                                                ebSemicolon = Nothing
                                              }
                                          ]
                                    ),
                                  AlphaNumBodyExpr $ mkRanged 21 22 23 24 "id2"
                                ],
                            ebSemicolon = Nothing
                          }
                      ]
              }
        ]
        [sbt|
            |1:2 - 23:24 function
            |	1:2 - 3:4 prefix function signature
            |		1:2 - 3:4 name
            |			1:2 - 3:4 alphnumeric identifier
            |				Name
            |	22:23 - 24:25 function body
            |		5:6 - 23:24 expressions block
            |			5:6 - 7:8 alphnumeric identifier
            |				id
            |			13:14 - 19:20 nested expressions
            |				13:14 - 19:20 expressions block
            |					13:14 - 15:16 alphnumeric identifier
            |						subId
            |					17:18 - 19:20 alphnumeric identifier
            |						subId2
            |			21:22 - 23:24 alphnumeric identifier
            |				id2
            |],
      testCaseParse
        "write a function without args which contains higher priority expressions"
        [ mkRanged 1 2 23 24 $
            Func
              { fSig = FuncSig {fsName = AlphaNumNameSigExpr $ mkRanged 1 2 3 4 "Name", fsParams = PrefixSigParams [], fsURL = ""},
                fBody =
                  FuncBody
                    $ mkRanged
                      25
                      26
                      27
                      28
                    $ NE.fromList
                      [ ExprsBlock
                          { ebExprs =
                              NE.fromList
                                [ AlphaNumBodyExpr $ mkRanged 5 6 7 8 "id",
                                  HigherPriorityExpr
                                    InBracketsNested
                                      { ibExprs =
                                          mkOcRanged
                                            13
                                            14
                                            15
                                            16
                                            $ NE.fromList
                                              [ ExprsBlock
                                                  { ebExprs =
                                                      NE.fromList
                                                        [ AlphaNumBodyExpr $ mkRanged 13 14 15 16 "subId",
                                                          AlphaNumBodyExpr $ mkRanged 17 18 19 20 "subId2"
                                                        ],
                                                    ebSemicolon = Nothing
                                                  }
                                              ],
                                        ibKind = RoundBrackets
                                      },
                                  AlphaNumBodyExpr $ mkRanged 21 22 23 24 "id2"
                                ],
                            ebSemicolon = Nothing
                          }
                      ]
              }
        ]
        [sbt|
            |1:2 - 23:24 function
            |	1:2 - 3:4 prefix function signature
            |		1:2 - 3:4 name
            |			1:2 - 3:4 alphnumeric identifier
            |				Name
            |	25:26 - 27:28 function body
            |		5:6 - 23:24 expressions block
            |			5:6 - 7:8 alphnumeric identifier
            |				id
            |			13:14 - 15:16 higher priority expressions
            |				13:14 - 19:20 expressions block
            |					13:14 - 15:16 alphnumeric identifier
            |						subId
            |					17:18 - 19:20 alphnumeric identifier
            |						subId2
            |			21:22 - 23:24 alphnumeric identifier
            |				id2
            |]
    ]
