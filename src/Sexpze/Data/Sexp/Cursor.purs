-- | A cursor into a s-expression.
module Sexpze.Data.Sexp.Cursor where

import Prelude

import Control.Plus (empty)
import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Sexpze.Data.Sexp (Sexp, Sexp'(..))
import Sexpze.Utility (bug, todo)

--------------------------------------------------------------------------------
-- traverseSexpWithCursor
--------------------------------------------------------------------------------

type TraverseSexpWithCursorRecs a r =
  { atom :: { cursorStatus :: Maybe CursorStatus, here :: Cursor } -> a -> r
  , group :: { cursorStatus :: Maybe CursorStatus, here :: Cursor } -> Array ({ before :: Cursor, x :: Sexp' a, r :: r }) -> { last :: Cursor } -> r
  }

traverseSexpWithCursor
  :: forall a r
   . TraverseSexpWithCursorRecs a r
  -> List Int
  -> Maybe SubCursor
  -> Sexp a
  -> Array r
traverseSexpWithCursor recs is mb_subcursor =
  Array.mapWithIndex \i x ->
    traverseSexp'WithCursor recs (is `List.snoc` i) (mb_subcursor >>= matchStepSubCursor i) x

traverseSexp'WithCursor
  :: forall a r
   . TraverseSexpWithCursorRecs a r
  -> List Int
  -> Maybe SubCursor
  -> Sexp' a
  -> r
traverseSexp'WithCursor { atom } is mb_subcursor (Atom a) =
  atom { cursorStatus: toCursorStatus =<< mb_subcursor, here: PointCursor (Point is) } a
traverseSexp'WithCursor recs@{ group } is mb_subcursor (Group xs) =
  group { cursorStatus: toCursorStatus =<< mb_subcursor, here: PointCursor (Point is) }
    ( Array.zipWith (\(i /\ x) r -> { before: PointCursor (Point (is `List.snoc` i)), x, r })
        (xs # Array.mapWithIndex Tuple)
        (xs # traverseSexpWithCursor recs is mb_subcursor)
    )
    { last: PointCursor (Point (is `List.snoc` Array.length xs)) }

--------------------------------------------------------------------------------
-- SubCursor
--------------------------------------------------------------------------------

type SubCursor = Cursor /\ SubCursorStatus

data SubCursorStatus
  = TopSubCursorStatus
  | SpanBeginSubCursorStatus
  | SpanEndSubCursorStatus
  | ZipperOuterBeginSubCursorStatus
  | ZipperOuterEndSubCursorStatus
  | ZipperInnerBeginSubCursorStatus
  | ZipperInnerEndSubCursorStatus

matchStepSubCursor :: Int -> SubCursor -> Maybe SubCursor
-- 
matchStepSubCursor j (PointCursor (Point (Cons i is')) /\ scs) | i == j = pure (PointCursor (Point is') /\ scs)
matchStepSubCursor _ (PointCursor (Point Nil) /\ _) = bug $ "matchStepSubCursor: tried to step into a PointCursor with no steps left"
-- 
matchStepSubCursor j (SpanCursor (Span { p0: Point (Cons i is') }) /\ TopSubCursorStatus) | i == j = pure (PointCursor (Point is') /\ SpanBeginSubCursorStatus)
matchStepSubCursor j (SpanCursor (Span { p1: Point (Cons i is') }) /\ TopSubCursorStatus) | i == j = pure (PointCursor (Point is') /\ SpanEndSubCursorStatus)
matchStepSubCursor _ (SpanCursor _ /\ _) = bug $ "matchStepSubCursor: tries to step into a non-PointCursor with non-TopSubCursorStatus"
-- 
matchStepSubCursor j (ZipperCursor (Zipper { s1: Span { p0: Point (Cons i is') } }) /\ TopSubCursorStatus) | i == j = pure (PointCursor (Point is') /\ ZipperOuterBeginSubCursorStatus)
matchStepSubCursor j (ZipperCursor (Zipper { s1: Span { p0: Point (Cons i is') } }) /\ TopSubCursorStatus) | i == j = pure (PointCursor (Point is') /\ ZipperOuterEndSubCursorStatus)
matchStepSubCursor j (ZipperCursor (Zipper { s2: Span { p1: Point (Cons i is') } }) /\ TopSubCursorStatus) | i == j = pure (PointCursor (Point is') /\ ZipperInnerBeginSubCursorStatus)
matchStepSubCursor j (ZipperCursor (Zipper { s2: Span { p1: Point (Cons i is') } }) /\ TopSubCursorStatus) | i == j = pure (PointCursor (Point is') /\ ZipperInnerEndSubCursorStatus)
matchStepSubCursor _ (ZipperCursor _ /\ _) = bug $ "matchStepSubCursor: tries to step into a non-PointCursor with non-TopSubCursorStatus"
-- 
matchStepSubCursor _ _ = empty

--------------------------------------------------------------------------------
-- CursorStatus
--------------------------------------------------------------------------------

data CursorStatus
  = PointCursorStatus
  | SpanBeginCursorStatus
  | SpanEndCursorStatus
  | ZipperOuterBeginCursorStatus
  | ZipperOuterEndCursorStatus
  | ZipperInnerBeginCursorStatus
  | ZipperInnerEndCursorStatus

toCursorStatus :: SubCursor -> Maybe CursorStatus
toCursorStatus (PointCursor (Point Nil) /\ TopSubCursorStatus) = pure PointCursorStatus
toCursorStatus (PointCursor (Point Nil) /\ SpanBeginSubCursorStatus) = pure SpanBeginCursorStatus
toCursorStatus (PointCursor (Point Nil) /\ SpanEndSubCursorStatus) = pure SpanEndCursorStatus
toCursorStatus (PointCursor (Point Nil) /\ ZipperOuterBeginSubCursorStatus) = pure ZipperOuterBeginCursorStatus
toCursorStatus (PointCursor (Point Nil) /\ ZipperOuterEndSubCursorStatus) = pure ZipperOuterEndCursorStatus
toCursorStatus (PointCursor (Point Nil) /\ ZipperInnerBeginSubCursorStatus) = pure ZipperInnerBeginCursorStatus
toCursorStatus (PointCursor (Point Nil) /\ ZipperInnerEndSubCursorStatus) = pure ZipperInnerEndCursorStatus
toCursorStatus _ = empty

--------------------------------------------------------------------------------
-- Cursor
--------------------------------------------------------------------------------

data Cursor
  = PointCursor Point
  | SpanCursor Span
  | ZipperCursor Zipper

-- moveLeft_Cursor :: forall a. Sexp a -> Cursor -> Maybe Cursor
-- moveLeft_Cursor x (PointCursor p) = PointCursor <$> moveLeft_Point x p
-- moveLeft_Cursor _ _ = todo "moveLeft_Cursor" {}

-- moveRight_Cursor :: forall a. Sexp a -> Cursor -> Maybe Cursor
-- moveRight_Cursor x (PointCursor p) = PointCursor <$> moveRight_Point x p
-- moveRight_Cursor _ _ = todo "moveRight_Cursor" {}

--------------------------------------------------------------------------------
-- Point
--------------------------------------------------------------------------------

-- | A `Point` is either a position `Between` two elements of a `Group`. It is
-- | encoded by a top-down index into a `Sexp`.
newtype Point = Point (List Int)

topPoint :: Point
topPoint = Point mempty

consPoint :: Int -> Point -> Point
consPoint i (Point is) = Point (i : is)

-- moveLeft_Point :: forall a. Sexp a -> Point -> Maybe Point
-- moveLeft_Point (Atom _) _ = empty
-- moveLeft_Point (Group xs) (Point Nil j) =
--   if not (0 <= j && j <= Array.length xs) then bug "invalid Point tip"
--   else if j == 0 then empty
--   else pure (Point Nil (j - 1))

-- moveLeft_Point (Group xs) (Point (i : is) j) =
--   let
--     x = xs Array.!! i # fromMaybe' \_ -> bug "invalid Point step"
--   in
--     consPoint i <$> moveLeft_Point x (Point is j)

-- moveRight_Point :: forall a. Sexp a -> Point -> Maybe Point
-- moveRight_Point (Atom _) _ = empty
-- moveRight_Point (Group xs) (Point Nil j) =
--   if not (0 <= j && j <= Array.length xs) then bug "invalid Point tip"
--   else if j == Array.length xs then empty
--   else pure (Point Nil (j + 1))

-- moveRight_Point (Group xs) (Point (i : is) j) =
--   let
--     x = xs Array.!! i # fromMaybe' \_ -> bug "invalid Point step"
--   in
--     consPoint i <$> moveRight_Point x (Point is j)

--------------------------------------------------------------------------------
-- Span
--------------------------------------------------------------------------------

-- | A `Span` is a contiguous span between two `Point`s that contains matching
-- | numbers of opening and closing parentheses. A `Span` has an associated
-- | number for how many outer unclosed parentheses it has.
newtype Span = Span { p0 :: Point, p1 :: Point }

--------------------------------------------------------------------------------
-- Zipper
--------------------------------------------------------------------------------

-- | A `Zipper` is composed of two `Span`s that contains matching numbers of
-- | opening and closing parentheses. A `Zipper` has an associated number for
-- | how many outer unclosed parentheses it has.
data Zipper = Zipper { s1 :: Span, s2 :: Span }

