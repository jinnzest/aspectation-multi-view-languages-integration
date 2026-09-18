module Pure.Main.Syntax.Lexer.Parsing.LexerGeneratorTest
  ( lexerGeneratorTest,
  )
where

import Control.Monad (Monad (return))
import Data.Bool ((||))
import Data.Char (isDigit)
import Data.Either (Either (Left, Right))
import Data.Eq (Eq ((==)))
import Data.Function (($))
import Data.Int (Int)
import Data.Kind (Type)
import Data.List as L (all, concatMap, map, (++))
import Data.Maybe (Maybe (Just, Nothing))
import Data.Monoid (Monoid (mconcat))
import Data.Text as T (Text, pack, replicate, singleton, splitOn)
import GHC.Integer (Integer, absInteger)
import Main.Syntax.Lexer.Parsing.Lexer (lexing)
import Main.Syntax.Lexer.Writing.Ranged as R (writeFuncs)
import Main.Syntax.Lexer.Writing.Source as F (writeFuncs)
import Pure.Main.Syntax.Shared (runLexer)
import Shared.Conditional.Debug.Trace (trace)
import Shared.Errors (Errors (Errors, errors))
import Shared.Location.Data (BracketsKind (CurlyBrackets, RoundBrackets, SquareBrackets))
import Shared.Text.Utils (nL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    arbitraryPrintableChar,
    elements,
    oneof,
    testProperty,
    vectorOf,
  )
import Text.Shakespeare.Text (st)
import Text.Show (Show, show)

genSize :: Gen Int
genSize = elements [1 .. 3]

genSpecIdentifier :: Gen Text
genSpecIdentifier = do
  size <- genSize
  id <-
    vectorOf size $
      oneof
        [ return '+',
          return '-',
          return '=',
          return '*',
          return '/',
          return '^',
          return '&',
          return ',',
          return '<',
          return '>',
          return '|',
          return '?',
          return '\'',
          return '%',
          return ':',
          return '.',
          return '~',
          return '!',
          return '@',
          return '$',
          return '`',
          return '\\'
        ]
  return $
    pack $
      if id == "->"
        then "-->"
        else id

splitter :: Text
splitter = [st|#{nL}######################################################################{nL}|]

genTabs :: Gen Text
genTabs = do
  size <- genSize
  tabs <- vectorOf size $ return $ singleton '\t'
  return $ mconcat tabs

genSingleSpaces :: Gen Text
genSingleSpaces = do
  size <- genSize
  tabs <- vectorOf size $ return " "
  return $ mconcat tabs

genHSpaces :: Gen Text
genHSpaces = oneof [return "", genSingleSpaces, genTabs]

genSpaces :: Gen Text
genSpaces =
  let tab = singleton '\t'
   in oneof [return [st|#{nL}#{tab}|], genHSpaces]

genAlphaNumIdentifier :: Gen Text
genAlphaNumIdentifier = do
  alphaSize <- genSize
  alphaNumId <- vectorOf alphaSize $ elements ('_' : (['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9']))
  let alphaNumIdWithDblE = L.concatMap (\c -> if c == 'e' || c == 'E' then [c, c] else [c]) alphaNumId
  if L.all (\c -> (c == '_') || isDigit c) alphaNumIdWithDblE
    then do
      alphaOnly <- vectorOf alphaSize $ elements (['A' .. 'Z'] ++ ['a' .. 'z'])
      return $ pack $ alphaNumIdWithDblE ++ alphaOnly
    else return $ pack alphaNumIdWithDblE

genParam :: Gen Text
genParam = do
  param <-
    oneof
      [ genAlphaNumIdentifier,
        genText,
        genNumber
      ]
  return [st| #{param}|]

genParamWithSpaces :: Gen Text
genParamWithSpaces = do
  spaces <- genHSpaces
  param <- oneof [genParam, genParamInBrackets]
  return [st|#{spaces}#{param}|]

genInfixSpecHoParametrizedParam :: Gen Text
genInfixSpecHoParametrizedParam = do
  spaces1 <- genHSpaces
  spaces2 <- genHSpaces
  spaces3 <- genHSpaces
  spaces4 <- genHSpaces
  name <- genSpecIdentifier
  return [st|#{spaces1}_#{spaces2}#{name}#{spaces3}_#{spaces4}|]

genInfixAlphanumHoParametrizedParam :: Gen Text
genInfixAlphanumHoParametrizedParam = do
  spaces1 <- genHSpaces
  spaces2 <- genHSpaces
  spaces3 <- genHSpaces
  spaces4 <- genHSpaces
  name <- genAlphaNumIdentifier
  return [st|#{spaces1}_#{spaces2} #{name} #{spaces3}_#{spaces4}|]

genPrefixHoParametrizedParam :: Gen Text
genPrefixHoParametrizedParam = do
  size <- genSize
  name <- oneof [genAlphaNumIdentifier, genSpecIdentifier]
  spaces <- genHSpaces
  let params = replicate size "_ "
  return [st|#{name}#{spaces} #{params}|]

genParamInBrackets :: Gen Text
genParamInBrackets = do
  expression <-
    oneof
      [ genTextWithSpaces,
        genNumberWithSpaces,
        genAlphaNumWithSpaces,
        genPrefixHoParametrizedParam,
        genInfixAlphanumHoParametrizedParam,
        genInfixSpecHoParametrizedParam
      ]
  return [st|(#{expression})|]

genParamInBracketsWithSpaces :: Gen Text
genParamInBracketsWithSpaces = do
  spaces <- genSpaces
  expr <- genParamInBrackets
  return [st|#{spaces}#{expr}|]

genPrefixFuncSig :: Gen Text
genPrefixFuncSig = do
  size <- genSize
  name <- oneof [genAlphaNumIdentifier, genSpecIdentifier]
  params <- vectorOf size genParamWithSpaces
  return [st|#{name}#{mconcat $ params}|]

infixFuncSig :: Gen Text
infixFuncSig = do
  spaces <- genHSpaces
  name <- oneof [genAlphaNumIdentifier, genSpecIdentifier]
  leftParam <- genParamInBrackets
  rightParam <- genParamInBracketsWithSpaces
  return [st|#{leftParam}#{spaces}#{name}#{rightParam}|]

genFuncSig :: Gen Text
genFuncSig =
  oneof
    [ infixFuncSig,
      genPrefixFuncSig
    ]

genNumber :: Gen Text
genNumber = do
  int <- arbitrary
  dec <- arbitrary
  let intText = show (int :: Integer)
  let decText = case dec :: Maybe Integer of
        Nothing -> ""
        Just d -> [st|.#{show $ absInteger d}|]
  exp <- arbitrary
  let expText = case exp :: Maybe Integer of
        Nothing -> ""
        Just e -> [st|e#{show e}|]
  return [st|#{intText}#{decText}#{expText}|]

genNumberWithSpaces :: Gen Text
genNumberWithSpaces = do
  spaces <- genSpaces
  number <- genNumber
  return [st|#{spaces} #{number}|]

genText :: Gen Text
genText = do
  size <- genSize
  textParts <- vectorOf size $ do
    ch <- arbitraryPrintableChar
    return $ if ch == '"' then [st|""|] else [st|#{show ch}|]
  return [st|"#{mconcat textParts}"|]

genTextWithSpaces :: Gen Text
genTextWithSpaces = do
  spaces <- genSpaces
  text <- genText
  return [st|#{spaces}#{text}|]

genAlphaNumWithSpaces :: Gen Text
genAlphaNumWithSpaces = do
  spaces <- genSpaces
  alphaNum <- genAlphaNumIdentifier
  return [st|#{spaces} #{alphaNum}|]

genSpec :: Gen Text
genSpec = do
  spaces <- genSpaces
  id <- genSpecIdentifier
  if id == "->"
    then do
      id2 <- genSpecIdentifier
      return [st|#{spaces}#{id}#{id2}|]
    else return [st|#{spaces}#{id}|]

genBodySpec :: Gen Text
genBodySpec = do
  spaces <- genSpaces
  id <- oneof [genSpecIdentifier, return ";"]
  if id == "->"
    then do
      id2 <- oneof [genSpecIdentifier, return ";"]
      return [st|#{spaces}#{id}#{id2}|]
    else return [st|#{spaces}#{id}|]

genSpecWithSpaces :: Gen Text
genSpecWithSpaces = do
  spaces <- genSpaces
  special <- genBodySpec
  return [st|#{spaces}#{special}|]

genInBrackets :: BracketsKind -> Gen Text
genInBrackets bracketsKind = do
  expression <-
    oneof
      [genTextWithSpaces, genNumberWithSpaces, genAlphaNumWithSpaces, genSpecWithSpaces, return ""]
  case bracketsKind of
    RoundBrackets -> return [st|(#{expression})|]
    SquareBrackets -> return [st|[#{expression}]|]
    CurlyBrackets -> return [st|{#{expression}}|]

genInBracketsWithSpaces :: Gen Text
genInBracketsWithSpaces = do
  spaces <- genSpaces
  expr <- oneof [genInBrackets RoundBrackets, genInBrackets SquareBrackets, genInBrackets CurlyBrackets]
  return [st|#{spaces}#{expr}|]

genNumberFollowedBySpec :: Gen Text
genNumberFollowedBySpec = do
  spaces <- genSpaces
  number <- genNumber
  special <- genSpec
  return [st|#{spaces} #{number}#{special}|]

genSpecFollowedByNumber :: Gen Text
genSpecFollowedByNumber = do
  spaces <- genSpaces
  number <- genNumber
  let mant = splitOn "e" number
      spacesBeforeNum :: Text
      spacesBeforeNum = case mant of
        h : _ | h == "0" -> " "
        _ -> ""
  special <- genBodySpec
  return $
    if special == "-"
      then [st|#{spaces} +#{number}|]
      else [st|#{spaces} #{special}#{spacesBeforeNum}#{number}|]

genExpr :: Gen Text
genExpr = do
  spaces <- genSpaces
  expression <-
    oneof
      [genNumberFollowedBySpec, genSpecFollowedByNumber, genTextWithSpaces, genNumberWithSpaces, genAlphaNumWithSpaces, genSpecWithSpaces, genInBracketsWithSpaces]
  return [st|#{spaces}#{expression}|]

genFuncBody :: Gen Text
genFuncBody = do
  size <- genSize
  hSpaces <- genSpaces
  head <- genExpr
  expressionsTail <- vectorOf size $ do
    spaces <- genSpaces
    expr <- genExpr
    return [st|#{spaces}#{expr}|]
  let headText = [st|#{hSpaces}#{head}|]
  return $ mconcat $ headText : expressionsTail

genFunc :: Gen Text
genFunc = do
  functionSig <- genFuncSig
  functionBody <- genFuncBody
  beginSpaces <- genSpaces
  endSpaces <- genSpaces
  return [st|#{nL}#{functionSig}#{beginSpaces}->#{functionBody}#{endSpaces}|]

type Source :: Type
newtype Source = Source Text deriving stock (Show)

instance Arbitrary Source where
  arbitrary = do
    size <- genSize
    head <- genFunc
    constructs <- vectorOf size genFunc
    return $ Source $ mconcat $ head : map (\c -> [st|#{nL}#{nL}#{c}|]) constructs

lexerGeneratorTest :: TestTree
lexerGeneratorTest =
  testGroup
    "Syntax generator tests"
    [ testProperty "" $ \(Source source) ->
        let traceText = [st|Source:#{nL}#{source}|]
            parsedResult = trace traceText $ runLexer source (lexing "")
         in case parsedResult of
              Right (tree, trailingSpaces) ->
                let writtenBack = F.writeFuncs tree
                    writtenBackWithSpaces = [st|#{writtenBack}#{trailingSpaces}|]
                    rangedText = R.writeFuncs tree
                 in trace
                      [st|#{nL}source#{splitter}#{source}#{splitter}
                          #{nL}writtenBack#{splitter}#{writtenBackWithSpaces}#{splitter}
                          #{nL}ranged#{nL}#{rangedText}#{nL}#{splitter}
                      |]
                      $ source == [st|#{writtenBackWithSpaces}|]
              Left errors -> trace [st|ERRORS: #{errors}|] $ errors == Errors {errors = []}
    ]
