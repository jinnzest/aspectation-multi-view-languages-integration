module Main.Syntax.Lexer.Parsing.Layouting (mkLayouts) where

import Data.Bool (Bool (False, True), otherwise)
import Data.Eq ((==))
import Data.Foldable (Foldable (foldl))
import Data.Function (($), (.))
import Data.Int (Int)
import Data.Kind (Type)
import Data.List (concatMap, filter, init, map)
import Data.List as L (foldr, last, reverse, (++))
import Data.Maybe (Maybe (Just, Nothing))
import Data.Ord (Ord ((<), (<=), (>)))
import Data.Tuple (snd)
import Main.Syntax.Lexer.LexerModel
  ( BodyExpr (HigherPriorityExpr, SpecBodyExpr),
    ExprsBlock (ExprsBlock, ebExprs),
    Nested (IndentedNested),
    ebSemicolon,
  )
import Main.Syntax.Lexer.Parsing.Shared (getBodyExprRange)
import Shared.Location.Data (Position (column, line), Range (from, to), Ranged (Ranged, rItem))

-- import Text.Shakespeare.Text (st)
-- import Shared.Conditional.Debug.Trace (trace)

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
          then ExprsBlock {ebExprs = expr : exprs, ebSemicolon = Nothing} : tail
          else ExprsBlock {ebExprs = [expr], ebSemicolon = Nothing} : block
   in (currLineFrom, newBlock)

splitByLines :: [BodyExpr] -> [ExprsBlock]
splitByLines [] = []
splitByLines exprs =
  let lastExpr = last exprs
      initExprs = init exprs
      lastExprBlock = [ExprsBlock {ebExprs = [lastExpr], ebSemicolon = Nothing}]
      lineLastExprStarts = lineExprStarts lastExpr
      result = foldr foldByLines (lineLastExprStarts, lastExprBlock) initExprs
   in snd result

applyHeadOrZero :: (t -> Int) -> [t] -> Int
applyHeadOrZero _ [] = 0
applyHeadOrZero f (h : _) = f h

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
        processedExprsLine = lineExprEnds $ last processedBlock
        unprocessedIndent = applyHeadOrZero itemIndent unprocessedBlock
        filterOutNestedExpr = filter (\case (HigherPriorityExpr (IndentedNested _)) -> False; _ -> True)
        exprsBlockHeadIndent (ExprsBlock {ebExprs = exprs}) = applyHeadOrZero itemIndent $ filterOutNestedExpr exprs
        processedIndent
          | baseBlockLine == processedExprsLine = 0
          | otherwise = exprsBlockHeadIndent processedExprsHead
        layoutSmallerIndention =
          layoutingByIndent
            ctx
              { processed = [ExprsBlock {ebExprs = HigherPriorityExpr (IndentedNested (reverse processedExprs)) : unprocessedBlock, ebSemicolon = Nothing}],
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
              processedNested = reverse $ processed nested
              unprocessedNested = unprocessed nested
           in layoutingByIndent
                ctx
                  { baseBlockIndent,
                    processed = ExprsBlock {ebExprs = processedBlock ++ [HigherPriorityExpr (IndentedNested processedNested)], ebSemicolon = Nothing} : processedTail,
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
  ExprsBlock {ebExprs = [], ebSemicolon = Just r {rItem = ()}} : blocks
foldingExprBySemicolon [] expr =
  [ExprsBlock {ebExprs = [expr], ebSemicolon = Nothing}]
foldingExprBySemicolon (ExprsBlock {ebExprs = exprs, ebSemicolon = sc@(Just _)} : blocksTail) expr =
  ExprsBlock {ebExprs = expr : exprs, ebSemicolon = sc} : blocksTail
foldingExprBySemicolon (eb@ExprsBlock {ebExprs = exprs} : blocksTail) expr =
  eb {ebExprs = expr : exprs} : blocksTail

layoutingBlockBySemicolon :: ExprsBlock -> [ExprsBlock]
layoutingBlockBySemicolon ExprsBlock {ebExprs = ebExprs} =
  let blocks = foldl foldingExprBySemicolon [] ebExprs
      reversedBlocks = map (\eb@ExprsBlock {ebExprs = exprs} -> eb {ebExprs = reverse exprs}) blocks
   in reverse reversedBlocks

layoutingBlocksBySemicolon :: [ExprsBlock] -> [ExprsBlock]
layoutingBlocksBySemicolon = concatMap layoutingBlockBySemicolon

mkLayouts :: Int -> [BodyExpr] -> [ExprsBlock]
mkLayouts baseBlockLine expressions =
  layoutingBlocksBySemicolon $
    reverse $
      processed $
        layoutingByIndent
          LayoutingCtx
            { baseBlockIndent = 0,
              baseBlockLine,
              processed = [],
              unprocessed = splitByLines expressions
            }
