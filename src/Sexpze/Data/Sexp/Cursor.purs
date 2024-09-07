-- | A cursor into a s-expression.
module Sexpze.Data.Sexp.Cursor where

import Prelude

import Control.Plus (empty)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Debug as Debug
import Sexpze.Data.Sexp (Sexp, Sexp'(..))
import Sexpze.Utility (bug, todo)

--------------------------------------------------------------------------------
-- traverseSexpWithCursor
--------------------------------------------------------------------------------

type TraverseSexpWithCursorRecs a r =
  { atom :: a -> r
  , group :: Array ({ before :: Cursor, x :: Sexp' a, r :: r }) -> { last :: Cursor } -> r
  }

traverseSexpWithCursor
  :: forall a r
   . Show a
  => TraverseSexpWithCursorRecs a r
  -> List Int
  -> Maybe SubCursor
  -> Sexp a
  -> Array r
traverseSexpWithCursor recs is mb_subcursor sexp = Debug.trace ("[traverseSexpWithCursor] " <> show { mb_subcursor, sexp }) \_ -> sexp #
  Array.mapWithIndex \i x ->
    traverseSexp'WithCursor recs (is `List.snoc` i) (mb_subcursor >>= matchStepSubCursor i) x

traverseSexp'WithCursor
  :: forall a r
   . Show a
  => TraverseSexpWithCursorRecs a r
  -> List Int
  -> Maybe SubCursor
  -> Sexp' a
  -> r
traverseSexp'WithCursor { atom } _is mb_subcursor sexp'@(Atom a) = Debug.trace ("[traverseSexp'WithCursor] " <> show { mb_subcursor, sexp' }) \_ ->
  atom a
traverseSexp'WithCursor recs@{ group } is mb_subcursor sexp'@(Group xs) = Debug.trace ("[traverseSexp'WithCursor] " <> show { mb_subcursor, sexp' }) \_ ->
  group
    ( Array.zipWith (\(j /\ x) r -> { before: PointCursor (Point is j), x, r })
        (xs # Array.mapWithIndex Tuple)
        (xs # traverseSexpWithCursor recs is mb_subcursor)
    )
    { last: PointCursor (Point is (Array.length xs)) }

--------------------------------------------------------------------------------
-- SubCursor
--------------------------------------------------------------------------------

type SubCursor = Cursor /\ SubCursorStatus

data SubCursorStatus
  = TopSubCursorStatus
  | PointSubCursorStatus
  | SpanBeginSubCursorStatus
  | SpanEndSubCursorStatus
  | ZipperOuterBeginSubCursorStatus
  | ZipperOuterEndSubCursorStatus
  | ZipperInnerBeginSubCursorStatus
  | ZipperInnerEndSubCursorStatus

derive instance Generic SubCursorStatus _

instance Show SubCursorStatus where
  show x = genericShow x

matchStepSubCursor :: Int -> SubCursor -> Maybe SubCursor
-- 
matchStepSubCursor i' (PointCursor (Point (Cons i is') j) /\ scs) | i == i' = pure (PointCursor (Point is' j) /\ scs)
-- 
matchStepSubCursor i' (SpanCursor (Span { p0: Point (Cons i is') j }) /\ TopSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ SpanBeginSubCursorStatus)
matchStepSubCursor i' (SpanCursor (Span { p1: Point (Cons i is') j }) /\ TopSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ SpanEndSubCursorStatus)
-- 
matchStepSubCursor i' (ZipperCursor (Zipper { s1: Span { p0: Point (Cons i is') j } }) /\ TopSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ ZipperOuterBeginSubCursorStatus)
matchStepSubCursor i' (ZipperCursor (Zipper { s1: Span { p0: Point (Cons i is') j } }) /\ TopSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ ZipperOuterEndSubCursorStatus)
matchStepSubCursor i' (ZipperCursor (Zipper { s2: Span { p1: Point (Cons i is') j } }) /\ TopSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ ZipperInnerBeginSubCursorStatus)
matchStepSubCursor i' (ZipperCursor (Zipper { s2: Span { p1: Point (Cons i is') j } }) /\ TopSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ ZipperInnerEndSubCursorStatus)
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

derive instance Generic CursorStatus _

instance Show CursorStatus where
  show x = genericShow x

-- toCursorStatus :: SubCursor -> Maybe CursorStatus
-- toCursorStatus (PointCursor (Point Nil _) /\ PointSubCursorStatus) = pure PointCursorStatus
-- toCursorStatus (PointCursor (Point Nil _) /\ SpanBeginSubCursorStatus) = pure SpanBeginCursorStatus
-- toCursorStatus (PointCursor (Point Nil _) /\ SpanEndSubCursorStatus) = pure SpanEndCursorStatus
-- toCursorStatus (PointCursor (Point Nil _) /\ ZipperOuterBeginSubCursorStatus) = pure ZipperOuterBeginCursorStatus
-- toCursorStatus (PointCursor (Point Nil _) /\ ZipperOuterEndSubCursorStatus) = pure ZipperOuterEndCursorStatus
-- toCursorStatus (PointCursor (Point Nil _) /\ ZipperInnerBeginSubCursorStatus) = pure ZipperInnerBeginCursorStatus
-- toCursorStatus (PointCursor (Point Nil _) /\ ZipperInnerEndSubCursorStatus) = pure ZipperInnerEndCursorStatus
-- toCursorStatus _ = empty

--------------------------------------------------------------------------------
-- Cursor
--------------------------------------------------------------------------------

data Cursor
  = PointCursor Point
  | SpanCursor Span
  | ZipperCursor Zipper

derive instance Generic Cursor _

instance Show Cursor where
  show x = genericShow x

-- moveLeft_Cursor :: forall a. Sexp a -> Cursor -> Maybe Cursor
-- moveLeft_Cursor x (PointCursor p) = PointCursor <$> moveLeft_Point x p
-- moveLeft_Cursor _ _ = todo "moveLeft_Cursor" {}

-- moveRight_Cursor :: forall a. Sexp a -> Cursor -> Maybe Cursor
-- moveRight_Cursor x (PointCursor p) = PointCursor <$> moveRight_Point x p
-- moveRight_Cursor _ _ = todo "moveRight_Cursor" {}

--------------------------------------------------------------------------------
-- Point
--------------------------------------------------------------------------------

-- | A `Point` is either a position `Between` two elements of a `Sexp`. It is
-- | encoded by a top-down index to a sub-`Sexp`, and then the index of a
-- | position between two elements of it.
data Point = Point (List Int) Int

derive instance Generic Point _

instance Show Point where
  show x = genericShow x

topPoint :: Point
topPoint = Point mempty 0

consPoint :: Int -> Point -> Point
consPoint i (Point is j) = Point (i : is) j

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

derive newtype instance Show Span

--------------------------------------------------------------------------------
-- Zipper
--------------------------------------------------------------------------------

-- | A `Zipper` is composed of two `Span`s that contains matching numbers of
-- | opening and closing parentheses. A `Zipper` has an associated number for
-- | how many outer unclosed parentheses it has.
newtype Zipper = Zipper { s1 :: Span, s2 :: Span }

derive newtype instance Show Zipper

