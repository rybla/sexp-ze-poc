-- | A cursor into a s-expression.
module Sexpze.Data.Sexp.Cursor where

import Prelude

import Control.Plus (empty)
import Data.Array as Array
import Data.List (List(..), (:))
import Data.Maybe (Maybe, fromMaybe')
import Sexpze.Data.Sexp (Sexp(..))
import Sexpze.Utility (bug, todo)

--------------------------------------------------------------------------------
-- Cursor
--------------------------------------------------------------------------------

data Cursor
  = PointCursor Point
  | SpanCursor Span
  | SelectCursor Select

moveLeft_Cursor :: forall a. Sexp a -> Cursor -> Maybe Cursor
moveLeft_Cursor x (PointCursor p) = PointCursor <$> moveLeft_Point x p
moveLeft_Cursor _ _ = todo "moveLeft_Cursor" {}

moveRight_Cursor :: forall a. Sexp a -> Cursor -> Maybe Cursor
moveRight_Cursor x (PointCursor p) = PointCursor <$> moveRight_Point x p
moveRight_Cursor _ _ = todo "moveRight_Cursor" {}

--------------------------------------------------------------------------------
-- Point
--------------------------------------------------------------------------------

-- | A `Point` is a position between two elements of a `Group`.
data Point = Point (List Int) Int

topPoint :: Point
topPoint = Point mempty 0

consPoint :: Int -> Point -> Point
consPoint i (Point is j) = Point (i : is) j

moveLeft_Point :: forall a. Sexp a -> Point -> Maybe Point
moveLeft_Point (Atom _) _ = empty
moveLeft_Point (Group xs) (Point Nil j) =
  if not (0 <= j && j <= Array.length xs) then bug "invalid Point tip"
  else if j == 0 then empty
  else pure (Point Nil (j - 1))

moveLeft_Point (Group xs) (Point (i : is) j) =
  let
    x = xs Array.!! i # fromMaybe' \_ -> bug "invalid Point step"
  in
    consPoint i <$> moveLeft_Point x (Point is j)

moveRight_Point :: forall a. Sexp a -> Point -> Maybe Point
moveRight_Point (Atom _) _ = empty
moveRight_Point (Group xs) (Point Nil j) =
  if not (0 <= j && j <= Array.length xs) then bug "invalid Point tip"
  else if j == Array.length xs then empty
  else pure (Point Nil (j + 1))

moveRight_Point (Group xs) (Point (i : is) j) =
  let
    x = xs Array.!! i # fromMaybe' \_ -> bug "invalid Point step"
  in
    consPoint i <$> moveRight_Point x (Point is j)

--------------------------------------------------------------------------------
-- Span
--------------------------------------------------------------------------------

-- | A `Span` is a contiguous span between two `Point`s that contains matching
-- | numbers of opening and closing parentheses. A `Span` has an associated
-- | number for how many outer unclosed parentheses it has.
newtype Span = Span { p0 :: Point, p1 :: Point }

--------------------------------------------------------------------------------
-- Select
--------------------------------------------------------------------------------

-- | A `Select` is composed of two `Span`s that contains matching numbers of
-- | opening and closing parentheses. A `Select` has an associated number for
-- | how many outer unclosed parentheses it has.
data Select = Select { s1 :: Span, s2 :: Span }

