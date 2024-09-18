-- TODO: make sure when insert span at span, update the end of span (since the point will be different now)
module Sexpze.Data.Sexp.Cursor.Drag where

import Prelude
import Sexpze.Data.Sexp
import Sexpze.Data.Sexp.Cursor

import Data.Either (Either(..))
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import Debug as Debug
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
        Cursor
          ( ZipperCursor
              (getSpanCursorBetweenPointIndices ph j2 j1 e)
              (SpanCursor mempty (wrap 0) (wrap 0))
          )
          (Inner Start)
      -- p1 is sibling and before p2
      Right j1 /\ Right j2 | j1 < j2 ->
        Cursor
          ( ZipperCursor
              (getSpanCursorBetweenPointIndices ph j1 j2 e)
              (SpanCursor mempty (wrap 0) (wrap 0))
          )
          (Inner Start)
      -- p1 is outside and before p2
      Right j_outer /\ Left (i_inner /\ _) | comparePointIndexToKidIndex' j_outer i_inner ->
        let
          PointCursor ph_inner j_inner = p2
          _ /\ e_inner = e # atPath ph_inner
        in
          Cursor
            ( ZipperCursor
                -- xxx is the point immediately to the right of i_inner
                (e # getSpanCursorBetweenPointIndices ph j_outer (i_inner # pointIndexRightAfterKidIndex))
                (e # getSpanCursorBetweenPointIndices ph_inner j_inner (lastPointIndexOfSpan e_inner))
            )
            (Inner Start)
      -- p1 is outside and after p2
      Right j_outer /\ Left (i_inner /\ _) | not $ comparePointIndexToKidIndex' j_outer i_inner ->
        let
          PointCursor ph_inner j_inner = p2
          _ /\ e_inner = e # atPath ph_inner
        in
          Cursor
            ( ZipperCursor
                (e # getSpanCursorBetweenPointIndices ph (j_outer # shiftPointIndexByPointDistNeg (wrap 1)) j_outer)
                (e # getSpanCursorBetweenPointIndices ph_inner (firstPointIndexOfSpan e_inner) j_inner)
            )
            (Inner End)
      -- p1 is inside and before p2
      Left (i_inner /\ _) /\ Right j_outer | compareKidIndexToPointIndex' i_inner j_outer ->
        let
          PointCursor ph_inner j_inner = p1
          _ /\ e_inner = e # atPath ph_inner
        in
          Cursor
            ( ZipperCursor
                (e # getSpanCursorBetweenPointIndices ph (j_outer # shiftPointIndexByPointDistNeg (wrap 1)) j_outer)
                (e # getSpanCursorBetweenPointIndices ph_inner j_inner (lastPointIndexOfSpan e_inner))
            )
            (Outer End)
      -- p1 is inside and after p2
      Left (i_inner /\ _) /\ Right j_outer | not $ compareKidIndexToPointIndex' i_inner j_outer ->
        let
          PointCursor ph_inner j_inner = p1
          _ /\ e_inner = e # atPath ph_inner
        in
          Cursor
            ( ZipperCursor
                (e # getSpanCursorBetweenPointIndices ph j_outer (j_outer # shiftPointIndexByPointDist (wrap 1)))
                (e # getSpanCursorBetweenPointIndices ph_inner (firstPointIndexOfSpan e_inner) j_inner)
            )
            (Outer Start)
      -- Left (i1 /\ ph1) /\ Left (i2 /\ ph2) ->
      --   let
      --     _ = unit
      --   in
      --     Cursor
      --       ( ZipperCursor
      --           ?a
      --           ?a
      --       )
      --       ?a
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

