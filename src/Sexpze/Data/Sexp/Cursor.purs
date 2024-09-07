-- | A cursor into a s-expression.
module Sexpze.Data.Sexp.Cursor where

import Prelude

import Control.Plus (empty)
import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe, fromMaybe')
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Sexpze.Data.Sexp (Sexp(..), Sexp'(..))
import Sexpze.Utility (bug, todo)

--------------------------------------------------------------------------------
-- recursor
--------------------------------------------------------------------------------

type TraverseSexpWithCursorRecs a r =
  { atom :: { cursorStatus :: Maybe CursorStatus, here :: Cursor } -> a -> r
  , group :: { cursorStatus :: Maybe CursorStatus, here :: Cursor } -> Array ({ before :: Cursor, x :: Sexp' a, r :: r }) -> { last :: Cursor } -> r
  }

traverseSexpWithCursor
  :: forall a r
   . TraverseSexpWithCursorRecs a r
  -> List Int
  -> Maybe Cursor
  -> Sexp a
  -> Array r
traverseSexpWithCursor recs is mb_csr =
  Array.mapWithIndex \i x ->
    traverseSexp'WithCursor recs (is `List.snoc` i) (unconsStepOfCursor i =<< mb_csr) x

traverseSexp'WithCursor
  :: forall a r
   . TraverseSexpWithCursorRecs a r
  -> List Int
  -> Maybe Cursor
  -> Sexp' a
  -> r
traverseSexp'WithCursor { atom } is mb_csr (Atom a) =
  atom { cursorStatus: toCursorStatus =<< mb_csr, here: PointCursor (Point is On) } a
traverseSexp'WithCursor recs@{ group } is mb_csr (Group xs) =
  group { cursorStatus: toCursorStatus =<< mb_csr, here: PointCursor (Point is On) }
    ( Array.zipWith (\(i /\ x) r -> { before: PointCursor (Point (is `List.snoc` i) Between), x, r })
        (xs # Array.mapWithIndex Tuple)
        (xs # traverseSexpWithCursor recs is mb_csr)
    )
    { last: PointCursor (Point (is `List.snoc` Array.length xs) Between) }

-- | Checks if the next step of the `Cursor` can follow the 
unconsStepOfCursor :: Int -> Cursor -> Maybe Cursor
unconsStepOfCursor = todo "unconsStepOfCursor" {}

toCursorStatus :: Cursor -> Maybe CursorStatus
toCursorStatus = todo "toCursorStatus"

data CursorStatus
  = PointCursorStatus
  | SpanBeginCursorStatus
  | SpanEndCursorStatus
  | ZipperOuterBeginCursorStatus
  | ZipperOuterEndCursorStatus
  | ZipperInnerBeginCursorStatus
  | ZipperInnerEndCursorStatus

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

-- | A `Point` is a position between two elements of a `Group`.
data Point = Point (List Int) PointType
data PointType = On | Between

topPoint :: Point
topPoint = Point mempty On

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

--------------------------------------------------------------------------------
-- Zipper
--------------------------------------------------------------------------------

-- | A `Zipper` is composed of two `Span`s that contains matching numbers of
-- | opening and closing parentheses. A `Zipper` has an associated number for
-- | how many outer unclosed parentheses it has.
data Zipper = Zipper { s1 :: Span, s2 :: Span }

