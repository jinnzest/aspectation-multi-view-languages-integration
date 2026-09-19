module Main.Syntax.Lexer.Parsing.Layouting (mkLayouts) where

import Data.Bool (Bool (False, True), otherwise)
import Data.Eq ((==))
import Data.Foldable (Foldable (foldl))
import Data.Function (($), (.))
import Data.Int (Int)
import Data.Kind (Type)
import Data.List as L (concatMap, foldr, init, last, map, reverse)
import Data.List.NonEmpty as NE (NonEmpty, appendList, filter, fromList, head, last, reverse, singleton, (<|))
import Data.Maybe (Maybe (Just, Nothing))
import Data.Ord (Ord ((<), (<=), (>)))
import Data.Tuple (snd)
import Main.Syntax.Lexer.LexerModel
  ( BodyExpr (HigherPriorityExpr, SpecBodyExpr),
    ExprsBlock (ExprsBlock, ebExprs),
    Nested (EmptyNested, IndentedNested),
    ebSemicolon,
  )
import Main.Syntax.Lexer.Parsing.Shared (getBodyExprRange)
import Shared.Location.Data (Position (column, line), Range (from, to), Ranged (Ranged, rItem))

type LayoutingCtx :: Type
data LayoutingCtx = LayoutingCtx
  { baseBlockIndent :: Int,
    baseBlockLine :: Int,
    processed :: [ExprsBlock],
    unprocessed :: [ExprsBlock]
  }

lineExprStarts :: BodyExpr -> Int
lineExprStarts = line . from . getBodyExprRange

lineExprEnds :: BodyExpr -> Int
lineExprEnds = line . to . getBodyExprRange

foldByLines :: BodyExpr -> (Int, [ExprsBlock]) -> (Int, [ExprsBlock])
foldByLines _ (_, []) = (0, [])
foldByLines expr (lastLineFrom, block@(ExprsBlock {ebExprs = exprs} : tail)) =
  let currLineFrom = lineExprStarts expr
      currLineTo = lineExprEnds expr
      newBlock =
        if currLineTo == lastLineFrom
          then ExprsBlock {ebExprs = expr <| exprs, ebSemicolon = Nothing} : tail
          else ExprsBlock {ebExprs = singleton expr, ebSemicolon = Nothing} : block
   in (currLineFrom, newBlock)

splitByLines :: [BodyExpr] -> [ExprsBlock]
splitByLines [] = []
splitByLines exprs =
  let lastExpr = L.last exprs
      initExprs = L.init exprs
      lastExprBlock = [ExprsBlock {ebExprs = singleton lastExpr, ebSemicolon = Nothing}]
      lineLastExprStarts = lineExprStarts lastExpr
      result = foldr foldByLines (lineLastExprStarts, lastExprBlock) initExprs
   in snd result

layoutingByIndent :: LayoutingCtx -> LayoutingCtx
layoutingByIndent ctx@LayoutingCtx {unprocessed = []} = ctx
layoutingByIndent ctx@LayoutingCtx {processed = [], unprocessed = unprocessedHead : unprocessedTail} =
  layoutingByIndent ctx {processed = [unprocessedHead], unprocessed = unprocessedTail}
layoutingByIndent
  ctx@LayoutingCtx
    { baseBlockIndent,
      baseBlockLine,
      processed = processedExprs@(processedExprsHead@(ExprsBlock {ebExprs = processedBlock}) : processedTail),
      unprocessed = unprocessedExprs@(unprocessedHead@(ExprsBlock {ebExprs = unprocessedBlock}) : unprocessedTail)
    } =
    let itemIndent = column . from . getBodyExprRange
        processedExprsLine = lineExprEnds $ NE.last processedBlock
        unprocessedIndent = itemIndent $ NE.head unprocessedBlock
        filterOutNestedExpr = NE.filter (\case (HigherPriorityExpr (IndentedNested _)) -> False; _ -> True)
        exprsBlockHeadIndent (ExprsBlock {ebExprs}) =
          case filterOutNestedExpr ebExprs of
            [] -> 0
            headExpr : _ -> itemIndent headExpr
        processedIndent
          | baseBlockLine == processedExprsLine = 0
          | otherwise = exprsBlockHeadIndent processedExprsHead
        layoutSmallerIndention =
          layoutingByIndent
            ctx
              { processed = [ExprsBlock {ebExprs = HigherPriorityExpr (IndentedNested (NE.fromList $ L.reverse processedExprs)) <| unprocessedBlock, ebSemicolon = Nothing}],
                unprocessed = unprocessedTail
              }
        layoutEqualIndention =
          layoutingByIndent
            ctx
              { processed = unprocessedHead : processedExprs,
                unprocessed = unprocessedTail
              }
        layoutBiggerIndention =
          let nested =
                layoutingByIndent
                  ctx
                    { baseBlockIndent = processedIndent,
                      processed = [],
                      unprocessed = unprocessedExprs
                    }
              processedNested = L.reverse $ processed nested
              unprocessedNested = unprocessed nested
           in layoutingByIndent
                ctx
                  { baseBlockIndent,
                    processed = ExprsBlock {ebExprs = NE.appendList processedBlock [HigherPriorityExpr (IndentedNested (NE.fromList processedNested))], ebSemicolon = Nothing} : processedTail,
                    unprocessed = unprocessedNested
                  }
     in if unprocessedIndent <= baseBlockIndent
          then ctx
          else
            if unprocessedIndent > processedIndent
              then layoutBiggerIndention
              else
                if unprocessedIndent < processedIndent
                  then layoutSmallerIndention
                  else layoutEqualIndention

foldingExprBySemicolon :: [ExprsBlock] -> BodyExpr -> [ExprsBlock]
foldingExprBySemicolon blocks (SpecBodyExpr r@Ranged {rItem = ";"}) =
  ExprsBlock {ebExprs = singleton $ HigherPriorityExpr (EmptyNested r {rItem = ()}), ebSemicolon = Just r {rItem = ()}} : blocks
foldingExprBySemicolon [] expr =
  [ExprsBlock {ebExprs = NE.singleton expr, ebSemicolon = Nothing}]
foldingExprBySemicolon (ExprsBlock {ebExprs = exprs, ebSemicolon = sc@(Just _)} : blocksTail) expr =
  ExprsBlock {ebExprs = expr <| exprs, ebSemicolon = sc} : blocksTail
foldingExprBySemicolon (eb@ExprsBlock {ebExprs = exprs} : blocksTail) expr =
  eb {ebExprs = expr <| exprs} : blocksTail

layoutingBlockBySemicolon :: ExprsBlock -> [ExprsBlock]
layoutingBlockBySemicolon ExprsBlock {ebExprs = ebExprs} =
  let blocks = foldl foldingExprBySemicolon [] ebExprs
      reversedBlocks =
        L.map
          ( \eb@ExprsBlock {ebExprs = exprs} -> eb {ebExprs = NE.reverse exprs}
          )
          blocks
   in L.reverse reversedBlocks

layoutingBlocksBySemicolon :: [ExprsBlock] -> NonEmpty ExprsBlock
layoutingBlocksBySemicolon = NE.fromList . concatMap layoutingBlockBySemicolon

mkLayouts :: Int -> [BodyExpr] -> NonEmpty ExprsBlock
mkLayouts baseBlockLine expressions =
  layoutingBlocksBySemicolon $
    L.reverse $
      processed $
        layoutingByIndent
          LayoutingCtx
            { baseBlockIndent = 0,
              baseBlockLine,
              processed = [],
              unprocessed = splitByLines expressions
            }
