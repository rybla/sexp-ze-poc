-- | A cursor into a s-expression.
module Sexpze.Data.Sexp.Cursor where

import Prelude

import Control.Plus (empty)
import Data.Array (all)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty as List.NonEmpty
import Data.List.Types as ListTypes
import Data.Maybe (Maybe(..), fromMaybe', maybe')
import Data.NonEmpty (NonEmpty(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (snd, swap)
import Data.Tuple.Nested (type (/\), (/\))
import Sexpze.Data.Sexp (Sexp, Sexp'(..))
import Sexpze.Utility (bug, todo)

--------------------------------------------------------------------------------
-- traverseSexpWithCursor
--------------------------------------------------------------------------------

type TraverseSexpWithCursorRecs a r =
  { atom :: a -> r
  , group ::
      { first :: { point :: Point, status :: Maybe CursorStatus }
      , last :: { point :: Point, status :: Maybe CursorStatus }
      }
      -> r
      -> r
  , list ::
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
  -> Path
  -> Maybe SubCursor
  -> Sexp a
  -> r
traverseSexpWithCursor recs is mb_subcursor xs = traverseSexpWithCursor_helper recs is mb_subcursor xs # snd

traverseSexpWithCursor_helper
  :: forall a r
   . Show a
  => TraverseSexpWithCursorRecs a r
  -> Path
  -> Maybe SubCursor
  -> Sexp a
  -> { first :: { point :: Point, status :: Maybe CursorStatus }
     , last :: { point :: Point, status :: Maybe CursorStatus }
     } /\ r
traverseSexpWithCursor_helper recs is mb_subcursor xs =
  let
    l = Array.length xs
    first =
      { point: Point is 0
      , status: toCursorStatus 0 =<< mb_subcursor
      }
    last =
      { point: Point is l
      , status: toCursorStatus l =<< mb_subcursor
      }
  in
    { first, last } /\
      recs.list
        ( xs # Array.mapWithIndex \i x ->
            { before:
                { point: Point is i
                , status: toCursorStatus i =<< mb_subcursor
                }
            , x
            , r: traverseSexp'WithCursor recs (is `List.snoc` i) (matchStepSubCursor i =<< mb_subcursor) x
            }
        )
        { last }

traverseSexp'WithCursor
  :: forall a r
   . Show a
  => TraverseSexpWithCursorRecs a r
  -> Path
  -> Maybe SubCursor
  -> Sexp' a
  -> r
traverseSexp'WithCursor { atom } _is _mb_subcursor (Atom a) = atom a
traverseSexp'WithCursor recs@{ group } is mb_subcursor (Group xs) =
  let
    { first, last } /\ r = traverseSexpWithCursor_helper recs is mb_subcursor xs
  in
    group { first, last } r

--------------------------------------------------------------------------------
-- SubCursor
--------------------------------------------------------------------------------

type SubCursor = Cursor /\ SubCursorStatus

data SubCursorStatus
  = PointSubCursorStatus
  | SpanStartSubCursorStatus
  | SpanEndSubCursorStatus
  | ZipperOuterStartSubCursorStatus
  | ZipperStartSubCursorStatus
  | ZipperEndSubCursorStatus
  | ZipperInnerSubCursorStatus
  | ZipperOuterEndSubCursorStatus
  | ZipperInnerStartSubCursorStatus
  | ZipperInnerEndSubCursorStatus

derive instance Generic SubCursorStatus _

instance Show SubCursorStatus where
  show x = genericShow x

matchStepSubCursor :: Int -> SubCursor -> Maybe SubCursor
-- 
-- PointCursor
-- 
matchStepSubCursor i' (PointCursor (Point (i : is') j) /\ scs) | i == i' = pure (PointCursor (Point is' j) /\ scs)
-- 
-- SpanCursor
-- 
-- SpanCursor, PointSubCursorStatus
matchStepSubCursor i' (SpanCursor (Span (Point (i : is'0) j0) (Point (i_ : is'1) j1)) /\ PointSubCursorStatus) | i == i_ && i == i' = pure (SpanCursor (Span (Point is'0 j0) (Point is'1 j1)) /\ PointSubCursorStatus)
matchStepSubCursor i' (SpanCursor (Span (Point (i : is') j) _) /\ PointSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ SpanStartSubCursorStatus)
matchStepSubCursor i' (SpanCursor (Span _ (Point (i : is') j)) /\ PointSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ SpanEndSubCursorStatus)
-- SpanCursor, ZipperStartSubCursorStatus|ZipperEndSubCursorStatus
matchStepSubCursor i' (SpanCursor (Span (Point (i : is') j) _) /\ ZipperStartSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ ZipperOuterStartSubCursorStatus)
matchStepSubCursor i' (SpanCursor (Span _ (Point (i : is') j)) /\ ZipperStartSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ ZipperInnerStartSubCursorStatus)
matchStepSubCursor i' (SpanCursor (Span (Point (i : is') j) _) /\ ZipperEndSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ ZipperInnerEndSubCursorStatus)
matchStepSubCursor i' (SpanCursor (Span _ (Point (i : is') j)) /\ ZipperEndSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ ZipperOuterEndSubCursorStatus)
matchStepSubCursor i' (SpanCursor (Span (Point (i : is') j) _) /\ ZipperInnerSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ ZipperInnerStartSubCursorStatus)
matchStepSubCursor i' (SpanCursor (Span _ (Point (i : is') j)) /\ ZipperInnerSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ ZipperInnerEndSubCursorStatus)
-- 
-- ZipperCursor
-- 
-- ZipperCursor, PointSubCursorStatus
matchStepSubCursor i' (ZipperCursor (Zipper (Point (i0 : is'0) j0) (Point (i1 : is'1) j1) (Point (i2 : is'2) j2) (Point (i3 : is'3) j3)) /\ PointSubCursorStatus)
  | all (i' == _) [ i0, i1, i2, i3 ] =
      pure (ZipperCursor (Zipper (Point is'0 j0) (Point is'1 j1) (Point is'2 j2) (Point is'3 j3)) /\ PointSubCursorStatus)
matchStepSubCursor i' (ZipperCursor (Zipper (Point (i0 : is'0) j0) (Point (i1 : is'1) j1) _ _) /\ PointSubCursorStatus)
  | all (i' == _) [ i0, i1 ] =
      pure (SpanCursor (Span (Point is'0 j0) (Point is'1 j1)) /\ ZipperStartSubCursorStatus)
matchStepSubCursor i' (ZipperCursor (Zipper _ _ (Point (i2 : is'2) j2) (Point (i3 : is'3) j3)) /\ PointSubCursorStatus)
  | all (i' == _) [ i2, i3 ] =
      pure (SpanCursor (Span (Point is'2 j2) (Point is'3 j3)) /\ ZipperEndSubCursorStatus)
matchStepSubCursor i' (ZipperCursor (Zipper _ (Point (i1 : is'1) j1) (Point (i2 : is'2) j2) _) /\ PointSubCursorStatus)
  | all (i' == _) [ i1, i2 ] =
      pure (SpanCursor (Span (Point is'1 j1) (Point is'2 j2)) /\ ZipperInnerSubCursorStatus)
matchStepSubCursor i' (ZipperCursor (Zipper (Point (i : is') j) _ _ _) /\ PointSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ ZipperOuterStartSubCursorStatus)
matchStepSubCursor i' (ZipperCursor (Zipper _ (Point (i : is') j) _ _) /\ PointSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ ZipperInnerStartSubCursorStatus)
matchStepSubCursor i' (ZipperCursor (Zipper _ _ (Point (i : is') j) _) /\ PointSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ ZipperInnerEndSubCursorStatus)
matchStepSubCursor i' (ZipperCursor (Zipper _ _ _ (Point (i : is') j)) /\ PointSubCursorStatus) | i == i' = pure (PointCursor (Point is' j) /\ ZipperOuterEndSubCursorStatus)
--
matchStepSubCursor _ _ = empty

--------------------------------------------------------------------------------
-- CursorStatus
--------------------------------------------------------------------------------

data CursorStatus
  = PointCursorStatus
  | SpanStartCursorStatus
  | SpanEndCursorStatus
  | ZipperOuterStartCursorStatus
  | ZipperInnerStartCursorStatus
  | ZipperOuterEndCursorStatus
  | ZipperInnerEndCursorStatus

derive instance Generic CursorStatus _

instance Show CursorStatus where
  show x = genericShow x

toCursorStatus :: Int -> SubCursor -> Maybe CursorStatus
-- 
toCursorStatus j' (PointCursor (Point Nil j) /\ PointSubCursorStatus) | j == j' = pure PointCursorStatus
toCursorStatus j' (PointCursor (Point Nil j) /\ SpanStartSubCursorStatus) | j == j' = pure SpanStartCursorStatus
toCursorStatus j' (PointCursor (Point Nil j) /\ SpanEndSubCursorStatus) | j == j' = pure SpanEndCursorStatus
toCursorStatus j' (PointCursor (Point Nil j) /\ ZipperOuterStartSubCursorStatus) | j == j' = pure ZipperOuterStartCursorStatus
toCursorStatus j' (PointCursor (Point Nil j) /\ ZipperInnerStartSubCursorStatus) | j == j' = pure ZipperInnerStartCursorStatus
toCursorStatus j' (PointCursor (Point Nil j) /\ ZipperOuterEndSubCursorStatus) | j == j' = pure ZipperOuterEndCursorStatus
toCursorStatus j' (PointCursor (Point Nil j) /\ ZipperInnerEndSubCursorStatus) | j == j' = pure ZipperInnerEndCursorStatus
-- 
toCursorStatus j' (SpanCursor (Span (Point Nil j) _) /\ PointSubCursorStatus) | j == j' = pure SpanStartCursorStatus
toCursorStatus j' (SpanCursor (Span _ (Point Nil j)) /\ PointSubCursorStatus) | j == j' = pure SpanEndCursorStatus
toCursorStatus j' (SpanCursor (Span (Point Nil j) _) /\ ZipperStartSubCursorStatus) | j == j' = pure ZipperOuterStartCursorStatus
toCursorStatus j' (SpanCursor (Span _ (Point Nil j)) /\ ZipperStartSubCursorStatus) | j == j' = pure ZipperInnerStartCursorStatus
toCursorStatus j' (SpanCursor (Span (Point Nil j) _) /\ ZipperEndSubCursorStatus) | j == j' = pure ZipperInnerStartCursorStatus
toCursorStatus j' (SpanCursor (Span _ (Point Nil j)) /\ ZipperEndSubCursorStatus) | j == j' = pure ZipperOuterEndCursorStatus
toCursorStatus j' (SpanCursor (Span (Point Nil j) _) /\ ZipperInnerSubCursorStatus) | j == j' = pure ZipperInnerStartCursorStatus
toCursorStatus j' (SpanCursor (Span _ (Point Nil j)) /\ ZipperInnerSubCursorStatus) | j == j' = pure ZipperInnerEndCursorStatus
-- 
toCursorStatus j' (ZipperCursor (Zipper (Point Nil j) _ _ _) /\ PointSubCursorStatus) | j == j' = pure ZipperOuterStartCursorStatus
toCursorStatus j' (ZipperCursor (Zipper _ (Point Nil j) _ _) /\ PointSubCursorStatus) | j == j' = pure ZipperInnerStartCursorStatus
toCursorStatus j' (ZipperCursor (Zipper _ _ (Point Nil j) _) /\ PointSubCursorStatus) | j == j' = pure ZipperInnerEndCursorStatus
toCursorStatus j' (ZipperCursor (Zipper _ _ _ (Point Nil j)) /\ PointSubCursorStatus) | j == j' = pure ZipperOuterEndCursorStatus
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

-- | A top-down path from the root to a sub-expression.
type Path = List Int

-- | A `Point` is either a position `Between` two elements of a `Sexp`. It is
-- | encoded by a top-down path to a sub-`Sexp`, and then the index of a
-- | position between two elements of it.
data Point = Point Path Int

derive instance Generic Point _

instance Show Point where
  show x = genericShow x

instance Eq Point where
  eq x = genericEq x

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
-- TODO: refactor this to actually be a Path and two Ints, since the Path of each of these Points must be the SAME
data Span = Span Point Point

derive instance Generic Span _

instance Show Span where
  show x = genericShow x

--------------------------------------------------------------------------------
-- Zipper
--------------------------------------------------------------------------------

-- | A `Zipper` is composed of two `Span`s that contains matching numbers of
-- | opening and closing parentheses. A `Zipper` has an associated number for
-- | how many outer unclosed parentheses it has.
data Zipper = Zipper Point Point Point Point

derive instance Generic Zipper _

instance Show Zipper where
  show x = genericShow x

--------------------------------------------------------------------------------
-- operations
--------------------------------------------------------------------------------

getSubSexp :: forall a. Path -> Sexp a -> Sexp a
getSubSexp Nil xs = xs
getSubSexp (i : is) xs = case xs Array.!! i of
  Nothing -> bug "getSubSexp" "step index out of bounds"
  Just (Group xs') -> getSubSexp is xs'
  Just (Atom _) -> bug "getSubSexp" "step into an Atom"

getSubWrapAndSexpAtPath :: forall a. Path -> Sexp a -> (Sexp a -> Sexp a) /\ Sexp a
getSubWrapAndSexpAtPath Nil xs = identity /\ xs
getSubWrapAndSexpAtPath (i : is) xs = case xs Array.!! i of
  Nothing -> bug "getSubWrapAndSexpAtPath" "Path step index out of bounds"
  Just (Group ys) ->
    lmap
      (\wrap ys' -> wrap $ xs # Array.updateAt i (Group ys') # fromMaybe' (\_ -> bug "getSubWrapAndSexpAtPath" "Path step index out of bounds"))
      (getSubWrapAndSexpAtPath is ys)
  Just (Atom _) -> bug "getSubWrapAndSexpAtPath" "Path step into an Atom"

getSubWrapAtPoint :: forall a. Point -> Sexp a -> (Sexp a -> Sexp a)
getSubWrapAtPoint (Point is j) xs =
  let
    wrap_ys /\ ys = xs # getSubWrapAndSexpAtPath is
  in
    wrap_ys <<< \ys_middle ->
      let
        { before: ys_before, after: ys_after } = ys # Array.splitAt j
      in
        ys_before <> ys_middle <> ys_after

cursorBetweenPoints :: forall a. Sexp a -> Point -> Point -> Cursor
cursorBetweenPoints _ p p_ | p == p_ = PointCursor p
cursorBetweenPoints xs p p' =
  let
    path
      /\ { top: p0, sub: _p0_sub@(Point is0' _j0') }
      /\ { top: p1, sub: _p1_sub@(Point is1' _j1') } = longestCommonPath p p'
  in
    case is0' /\ is1' of
      Nil /\ Nil -> SpanCursor (Span p0 p1)
      Nil /\ (i1' : is1'_tail) ->
        -- zipper where p0 is outer and p1 is inner
        -- 
        let
          -- find the outermost Sexp between p0 and p1
          outermostSexp = getSubSexp (path `List.snoc` i1') xs
          -- find the innermost Sexp between p0 and p1
          -- innermost_group = List.NonEmpty.snoc' path i1' # List.NonEmpty.tail
          innermostSexp = getSubSexp (path <> is1') xs
        in
          --
          -- OuterStart is at p0
          -- InnerStart is at p1
          -- InnerEnd is at the last sibling position to p1
          -- InnerEnd is at the last position inside the innermost Sexp between p0 and p1
          -- OuterEnd is at the first sibling position to p0 that's after the outermost Sexp between p0 and p1
          ZipperCursor (Zipper p0 p1 (Point (path <> is1') (Array.length innermostSexp)) (Point path (i1' + 1)))
      (_ : _) /\ Nil -> PointCursor topPoint -- zipper wher p0 is inner and p1 is outer
      _ ->
        -- not a valid Span or Zipper between these two points directly, so just
        -- span to the smallest Span that contains both points
        let
          xs_sub = xs # getSubSexp path
        in
          SpanCursor (Span (Point path 0) (Point path (Array.length xs_sub)))

-- | Finds the longest common path (from the root) of the two points, and also
-- | orders the points by which appears first from left-to-right.
longestCommonPath
  :: Point
  -> Point
  -> Path /\ ({ top :: Point, sub :: Point } /\ { top :: Point, sub :: Point })
longestCommonPath p_top p'_top = go Nil p_top p'_top
  where
  go revPath p@(Point Nil j) p'@(Point Nil j') =
    List.reverse revPath /\ swapUnless (j < j') ({ top: p_top, sub: p } /\ { top: p'_top, sub: p' })
  go revPath p@(Point (Cons i _) _) p'@(Point Nil j') =
    List.reverse revPath /\ swapUnless (i < j') ({ top: p_top, sub: p } /\ { top: p'_top, sub: p' })
  go revPath p@(Point Nil j) p'@(Point (Cons i' _) _) =
    List.reverse revPath /\ swapUnless (j < i') ({ top: p_top, sub: p } /\ { top: p'_top, sub: p' })
  go revPath p@(Point (Cons i is) j) p'@(Point (Cons i' is') j') =
    if i /= i' then
      List.reverse revPath /\ swapUnless (i <= i') ({ top: p_top, sub: p } /\ { top: p'_top, sub: p' })
    else
      go (i : revPath) (Point is j) (Point is' j')

  swapUnless = if _ then identity else swap

--------------------------------------------------------------------------------
-- edits
--------------------------------------------------------------------------------

insertSexpAtPoint :: forall a. Point -> Sexp a -> Sexp a -> Maybe (Point /\ Sexp a)
insertSexpAtPoint point@(Point is j) ys xs = Point is (j + 1) /\ (getSubWrapAtPoint point xs) ys # pure
