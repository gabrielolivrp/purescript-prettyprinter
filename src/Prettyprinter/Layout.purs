module Prettyprinter.Layout where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Int (floor, toNumber)
import Prettyprinter.Doc (Doc(..))
import Prettyprinter.DocStream (DocStream(..))
import Prettyprinter.PageWidth (PageWidth(..))

newtype LayoutOptions = LayoutOptions
  { layoutPageWidth :: PageWidth
  }

data LayoutPipeline ann
  = Nil
  | Cons Int (Doc ann) (LayoutPipeline ann)
  | UndoAnn (LayoutPipeline ann)

newtype FittingPredicate ann
  = FittingPredicate
    (  Int
    -> Int
    -> Maybe Int
    -> DocStream ann
    -> Boolean
    )

defaultPageWidth :: PageWidth
defaultPageWidth = AvailablePerLine 30 0.5

defaultLayoutOptions :: LayoutOptions
defaultLayoutOptions = LayoutOptions { layoutPageWidth: defaultPageWidth }

layoutPretty :: forall ann. LayoutOptions -> Doc ann -> DocStream ann
layoutPretty (LayoutOptions { layoutPageWidth: availablePerLine@(AvailablePerLine lineLength ribbonFraction) }) =
  layoutWadlerLeijen (FittingPredicate fits) availablePerLine
  where
    fits lineIndent currentColumn _ ds =
      go (remainingWidth lineLength ribbonFraction lineIndent currentColumn) ds
      where
        go :: Int -> DocStream ann -> Boolean
        go w _ | w < 0 = false
        go _ SFail = false
        go _ SEmpty = true
        go w (SChar _ ds') = go (w - 1) ds'
        go w (SText l _ ds') = go (w - l) ds'
        go _ (SLine _ _) = true
        go w (SAnnPush _ ds') = go w ds'
        go w (SAnnPop ds') = go w ds'
layoutPretty (LayoutOptions { layoutPageWidth: Unbounded }) = layoutUnbounded

layoutSmart :: forall ann. LayoutOptions -> Doc ann -> DocStream ann
layoutSmart (LayoutOptions { layoutPageWidth: availablePerLine@(AvailablePerLine lineLength ribbonFraction) }) =
  layoutWadlerLeijen (FittingPredicate fits) availablePerLine
  where
    fits lineIndent currentColumn initialIndentationY = go availableWidth
      where
        go w _ | w < 0 = false
        go _ SFail = false
        go _ SEmpty = true
        go w (SChar _ ds') = go (w - 1) ds'
        go w (SText l _ ds') = go (w - l) ds'
        go _ (SLine i x)
          | minNestingLevel <= i = go (lineLength - i) x
          | otherwise = true
        go w (SAnnPush _ ds') = go w ds'
        go w (SAnnPop ds') = go w ds'

        availableWidth = remainingWidth lineLength ribbonFraction lineIndent currentColumn

        minNestingLevel = case initialIndentationY of
          Just i -> min i currentColumn
          Nothing -> currentColumn
layoutSmart (LayoutOptions { layoutPageWidth: Unbounded }) = layoutUnbounded

layoutUnbounded :: forall ann. Doc ann -> DocStream ann
layoutUnbounded = layoutWadlerLeijen (FittingPredicate fits) Unbounded
  where
    failsOnFirstLine SFail = true
    failsOnFirstLine SEmpty = false
    failsOnFirstLine (SChar _ ds) = failsOnFirstLine ds
    failsOnFirstLine (SText _ _ ds) = failsOnFirstLine ds
    failsOnFirstLine (SLine _ _) = false
    failsOnFirstLine (SAnnPush _ ds) = failsOnFirstLine ds
    failsOnFirstLine (SAnnPop ds) = failsOnFirstLine ds

    fits _ _ _ ds = not (failsOnFirstLine ds)

layoutWadlerLeijen :: forall ann. FittingPredicate ann -> PageWidth -> Doc ann -> DocStream ann
layoutWadlerLeijen (FittingPredicate fits) pageWidth doc = best 0 0 (Cons 0 doc Nil)
  where
    best _ _ Nil = SEmpty
    best nl cc (UndoAnn ds) = SAnnPop (best nl cc ds)
    best nl cc (Cons i d ds) =
      case d of
        Fail -> SFail
        Empty -> best nl cc ds
        Char c -> SChar c (best nl (cc + 1) ds)
        Text l t -> SText l t (best nl (cc + l) ds)
        Line -> let x = best i i ds
                    i' = case x of
                      SEmpty -> 0
                      SLine _ _ -> 0
                      _ -> i
                in SLine i' x
        FlatAlt x _ -> best nl cc (Cons i x ds)
        Cat x y -> best nl cc (Cons i x (Cons i y ds))
        Nest j x -> best nl cc (Cons (i + j) x ds)
        Union x y ->
          let bestX = best nl cc (Cons i x ds)
              bestY = best nl cc (Cons i y ds)
          in selectNicer nl cc bestX bestY
        Column f -> best nl cc (Cons i (f cc) ds)
        WithPageWidth f -> best nl cc (Cons i (f pageWidth) ds)
        Nesting f -> best nl cc (Cons i (f i) ds)
        Annotated ann x -> SAnnPush ann (best nl cc (Cons i x (UndoAnn ds)))


    selectNicer li cc x y
      | fits li cc (initialIndentation y) x = x
      | otherwise = y

    initialIndentation ds = case ds of
      SLine i _ -> Just i
      SAnnPush _ ds' -> initialIndentation ds'
      SAnnPop ds' -> initialIndentation ds'
      _ -> Nothing

remainingWidth :: Int -> Number -> Int -> Int -> Int
remainingWidth lineLength ribbonFraction lineIndent currentColumn = min columnsLeftInLine columnsLeftInRibbon
  where
    columnsLeftInLine = lineLength - currentColumn
    columnsLeftInRibbon = lineIndent + ribbonWidth - currentColumn
    ribbonWidth = (max 0 <<< min lineLength <<< floor) (toNumber lineLength * ribbonFraction)

layoutCompact :: forall ann1 ann2. Doc ann1 -> DocStream ann2
layoutCompact doc = scan 0 [doc]
  where
    scan :: Int -> Array (Doc ann1) -> DocStream ann2
    scan col docs =
      case A.uncons docs of
        Nothing -> SEmpty
        Just {head, tail} ->
          case head of
            Fail -> SFail
            Empty -> scan col tail
            Char c -> SChar c (scan (col+1) tail)
            Text l t -> SText l t (scan (col + l) tail)
            FlatAlt x _ -> scan col (x A.: tail)
            Line -> SLine 0 (scan 0 tail)
            Cat x y -> scan col (x A.: y A.: tail)
            Nest _ x -> scan col (x A.: tail)
            Union _ y -> scan col (y A.: tail)
            Column f -> scan col (f col A.: tail)
            WithPageWidth f -> scan col (f Unbounded A.: tail)
            Nesting f -> scan col (f 0 A.: tail)
            Annotated _ x -> scan col (x A.: tail)
