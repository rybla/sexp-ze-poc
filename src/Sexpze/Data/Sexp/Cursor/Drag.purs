-- TODO: make sure when insert span at span, update the end of span (since the point will be different now)
module Sexpze.Data.Sexp.Cursor.Drag where

import Prelude
import Sexpze.Data.Sexp
import Sexpze.Data.Sexp.Cursor

import Data.Either (Either(..))
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import Sexpze.Utility (bug, todo)

--------------------------------------------------------------------------------

dragFromPointCursor :: forall n a. PointCursor -> PointCursor -> Span n a -> Cursor
dragFromPointCursor p1_top p2_top e_top =
  let
    ph /\ p1 /\ p2 = commonPathOfPointCursors p1_top p2_top
    _ /\ e = atPath ph e_top
  in
    case unconsPointCursor p1 /\ unconsPointCursor p2 of
      -- p1 is sibling and before p2
      Right j1 /\ Right j2 | j1 <= j2 ->
        Cursor
          ( ZipperCursor
              (getSpanCursorBetweenPointIndices ph j1 j2 e)
              (SpanCursor mempty (wrap 0) (wrap 0))
          )
          (Inner End)
      -- p1 is sibling and after p2
      Right j1 /\ Right j2 | otherwise || j1 > j2 ->
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
                (e # getSpanCursorBetweenPointIndices ph j_outer (j_outer # shiftPointIndexByPointDist (wrap 1)))
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
                (e # getSpanCursorBetweenPointIndices ph j_outer (j_outer # shiftPointIndexByPointDist (wrap 1)))
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
                (e # getSpanCursorBetweenPointIndices ph j_outer (j_outer # shiftPointIndexByPointDist (wrap 1)))
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
      _ -> bug "dragFromPointCursor" "impossible"

dragFromSpanCursor :: forall n a. SpanCursor -> SpanCursor -> Span n a -> Cursor
dragFromSpanCursor = todo "dragFromSpan" {}

dragFromZipperCursor :: forall n a. ZipperCursor -> ZipperCursor -> Span n a -> Cursor
dragFromZipperCursor = todo "dragFromZipper" {}

dragFromCursor :: forall n a. Cursor -> Cursor -> Span n a -> Cursor
dragFromCursor = todo "dragFrom" {}

--------------------------------------------------------------------------------
-- misc
--------------------------------------------------------------------------------

