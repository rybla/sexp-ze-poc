module Sexpze.Data.Sexp.Cursor.Drag where

import Prelude
import Sexpze.Data.Sexp
import Sexpze.Data.Sexp.Cursor

import Data.Either (Either(..))
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import Sexpze.Utility (bug, todo)

--------------------------------------------------------------------------------

dragFromPointCursor :: forall n a. Show n => Show a => PointCursor -> PointCursor -> Span n a -> Cursor
dragFromPointCursor p1_top p2_top e_top =
  let
    ph /\ p1 /\ p2 = commonPathOfPointCursors p1_top p2_top
    _ /\ e = atPath ph e_top
  in

    case unconsPointCursor p1 /\ unconsPointCursor p2 of
      -- p1 is sibling and after p2
      Right j1 /\ Right j2 | j1 >= j2 ->
        let
          s_outer = getSpanCursorBetweenPointIndices ph j2 j1 e
          s_inner = SpanCursor mempty (wrap 0) (wrap 0)
        in
          Cursor (ZipperCursor s_outer s_inner) (Inner Start)
      -- p1 is sibling and before p2
      Right j1 /\ Right j2 | j1 < j2 ->
        let
          s_outer = getSpanCursorBetweenPointIndices ph j1 j2 e
          s_inner = SpanCursor mempty (wrap 0) (wrap 0)
        in
          Cursor (ZipperCursor s_outer s_inner) (Inner Start)
      -- p1 is outside and before p2
      Right j_outer /\ Left (i_inner /\ p'_inner) | comparePointIndexToKidIndex' j_outer i_inner ->
        let
          -- p_outer = p1
          p_inner = p2
          PointCursor ph_inner j_inner = p_inner
          PointCursor ph'_inner _ = p'_inner
          _ /\ e_inner = e # atPath ph_inner
          s_outer = e # getSpanCursorBetweenPointIndices ph j_outer (i_inner # pointIndexRightAfterKidIndex)
          s_inner = e # getSpanCursorBetweenPointIndices ((i_inner # trimKidIndexIntoSpanCursor s_outer) `consPath` ph'_inner) j_inner (lastPointIndexOfSpan e_inner)
        in
          Cursor (ZipperCursor s_outer s_inner) (Inner Start)
      -- p1 is outside and after p2
      Right j_outer /\ Left (i_inner /\ p'_inner) | not $ comparePointIndexToKidIndex' j_outer i_inner ->
        let
          -- p_outer = p1
          p_inner = p2
          PointCursor ph_inner j_inner = p_inner
          PointCursor ph'_inner _ = p'_inner
          _ /\ e_inner = e # atPath ph_inner
          s_outer = e # getSpanCursorBetweenPointIndices ph (i_inner # pointIndexRightBeforeKidIndex) j_outer
          s_inner = e # getSpanCursorBetweenPointIndices ((i_inner # trimKidIndexIntoSpanCursor s_outer) `consPath` ph'_inner) (firstPointIndexOfSpan e_inner) j_inner
        in
          Cursor (ZipperCursor s_outer s_inner) (Inner End)
      -- p1 is inside and before p2
      Left (i_inner /\ p'_inner) /\ Right j_outer | compareKidIndexToPointIndex' i_inner j_outer ->
        let
          p_inner = p1
          -- p_outer = p2
          PointCursor _ph_inner j_inner = p_inner
          PointCursor ph'_inner _ = p'_inner
          s_outer = e # getSpanCursorBetweenPointIndices ph (i_inner # pointIndexRightBeforeKidIndex) j_outer
          s_inner = e # getSpanCursorBetweenPointIndices ((i_inner # trimKidIndexIntoSpanCursor s_outer) `consPath` ph'_inner) j_inner j_inner
        in
          Cursor (ZipperCursor s_outer s_inner) (Outer End)
      -- p1 is inside and after p2
      Left (i_inner /\ p'_inner) /\ Right j_outer | not $ compareKidIndexToPointIndex' i_inner j_outer ->
        let
          -- p_outer = p2 
          p_inner = p1
          PointCursor _ph_inner j_inner = p_inner
          PointCursor ph'_inner _ = p'_inner
          s_outer = e # getSpanCursorBetweenPointIndices ph j_outer (i_inner # pointIndexRightAfterKidIndex)
          s_inner = e # getSpanCursorBetweenPointIndices ((i_inner # trimKidIndexIntoSpanCursor s_outer) `consPath` ph'_inner) j_inner j_inner
        in
          Cursor (ZipperCursor s_outer s_inner) (Outer Start)
      Left (_i1 /\ _ph1) /\ Left (_i2 /\ _ph2) -> bug "dragFromPointCursor" "is this impossible?"
      x1 /\ x2 -> bug "dragFromPointCursor" ("impossible: " <> show { x1, x2 })

dragFromSpanCursor :: forall n a. SpanCursor -> PointCursor -> Span n a -> Cursor
dragFromSpanCursor _ _ _ = todo "dragFromSpanCursor" {}

dragFromZipperCursor :: forall n a. ZipperCursor -> PointCursor -> Span n a -> Cursor
dragFromZipperCursor _ _ _ = todo "dragFromZipperCursor" {}

dragFromCursor :: forall n a. Show n => Show a => Cursor -> PointCursor -> Span n a -> Cursor
-- dragFromCursor c p = todo "dragFromCursor" {}
dragFromCursor c p e = dragFromPointCursor (getCursorHandle c e) p e

--------------------------------------------------------------------------------
-- misc
--------------------------------------------------------------------------------

