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
import Sexpze.Data.Sexp (Sexp, Sexp'(..))

--------------------------------------------------------------------------------
-- traverseSexpWithCursor
--------------------------------------------------------------------------------

type TraverseSexpWithCursorRecs a r =
  { atom :: a -> r
  , group ::
      Array
        ( { before :: { point :: Point, status :: Maybe CursorStatus }
          , x :: Sexp' a
          , r :: r
          }
        )
      -> { last :: { point :: Point, status :: Maybe CursorStatus } }
      -> r
  }

traverseSexpWithCursor
  :: forall a r
   . Show a
  => TraverseSexpWithCursorRecs a r
  -> List Int
  -> Maybe SubCursor
  -> Sexp a
  -> Array r
-- traverseSexpWithCursor recs is mb_subcursor sexp = Debug.trace ("[traverseSexpWithCursor] " <> show { mb_subcursor, sexp }) \_ -> sexp #
traverseSexpWithCursor recs is mb_subcursor sexp = sexp #
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
-- traverseSexp'WithCursor { atom } _is mb_subcursor sexp'@(Atom a) = Debug.trace ("[traverseSexp'WithCursor] " <> show { mb_subcursor, sexp' }) \_ ->
traverseSexp'WithCursor { atom } _is mb_subcursor sexp'@(Atom a) =
  atom a
-- traverseSexp'WithCursor recs@{ group } is mb_subcursor sexp'@(Group xs) = Debug.trace ("[traverseSexp'WithCursor] " <> show { mb_subcursor, sexp' }) \_ ->
traverseSexp'WithCursor recs@{ group } is mb_subcursor sexp'@(Group xs) =
  group
    ( Array.zipWith
        ( \(j /\ x) r ->
            { before:
                { point: Point is j
                , status: toCursorStatus j =<< mb_subcursor
                }
            , x
            , r
            }
        )
        (xs # Array.mapWithIndex Tuple)
        (xs # traverseSexpWithCursor recs is mb_subcursor)
    )
    { last:
        { point: Point is (Array.length xs)
        , status: toCursorStatus (Array.length xs) =<< mb_subcursor
        }
    }

--------------------------------------------------------------------------------
-- SubCursor
--------------------------------------------------------------------------------

type SubCursor = Cursor /\ SubCursorStatus

data SubCursorStatus
  = PointSubCursorStatus
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
matchStepSubCursor i' (SpanCursor (Span { p0: Point (Cons i is'1) j1, p1: Point (Cons i_ is'2) j2 }) /\ PointSubCursorStatus) | i == i_ && i == i' = pure (SpanCursor (Span { p0: Point is'1 j1, p1: Point is'2 j2 }) /\ PointSubCursorStatus)
matchStepSubCursor i' (SpanCursor (Span { p0: Point (Cons i is') j }) /\ PointSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ SpanBeginSubCursorStatus)
matchStepSubCursor i' (SpanCursor (Span { p1: Point (Cons i is') j }) /\ PointSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ SpanEndSubCursorStatus)
-- 
matchStepSubCursor i' (ZipperCursor (Zipper { s1: Span { p0: Point (Cons i is') j } }) /\ PointSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ ZipperOuterBeginSubCursorStatus)
matchStepSubCursor i' (ZipperCursor (Zipper { s1: Span { p0: Point (Cons i is') j } }) /\ PointSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ ZipperOuterEndSubCursorStatus)
matchStepSubCursor i' (ZipperCursor (Zipper { s2: Span { p1: Point (Cons i is') j } }) /\ PointSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ ZipperInnerBeginSubCursorStatus)
matchStepSubCursor i' (ZipperCursor (Zipper { s2: Span { p1: Point (Cons i is') j } }) /\ PointSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ ZipperInnerEndSubCursorStatus)
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

toCursorStatus :: Int -> SubCursor -> Maybe CursorStatus
toCursorStatus j' (PointCursor (Point Nil j) /\ PointSubCursorStatus) | j == j' = pure PointCursorStatus
toCursorStatus j' (PointCursor (Point Nil j) /\ SpanBeginSubCursorStatus) | j == j' = pure SpanBeginCursorStatus
toCursorStatus j' (PointCursor (Point Nil j) /\ SpanEndSubCursorStatus) | j == j' = pure SpanEndCursorStatus
toCursorStatus j' (PointCursor (Point Nil j) /\ ZipperOuterBeginSubCursorStatus) | j == j' = pure ZipperOuterBeginCursorStatus
toCursorStatus j' (PointCursor (Point Nil j) /\ ZipperOuterEndSubCursorStatus) | j == j' = pure ZipperOuterEndCursorStatus
toCursorStatus j' (PointCursor (Point Nil j) /\ ZipperInnerBeginSubCursorStatus) | j == j' = pure ZipperInnerBeginCursorStatus
toCursorStatus j' (PointCursor (Point Nil j) /\ ZipperInnerEndSubCursorStatus) | j == j' = pure ZipperInnerEndCursorStatus
toCursorStatus j' (SpanCursor (Span { p0: Point Nil j }) /\ PointSubCursorStatus) | j == j' = pure SpanBeginCursorStatus
toCursorStatus j' (SpanCursor (Span { p1: Point Nil j }) /\ PointSubCursorStatus) | j == j' = pure SpanEndCursorStatus
-- TODO: same for Zipper as for Span above
toCursorStatus _ _ = empty

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

