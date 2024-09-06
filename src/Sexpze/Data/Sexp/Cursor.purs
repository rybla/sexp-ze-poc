-- | A cursor into a s-expression.
module Sexpze.Data.Sexp.Cursor where

import Prelude

import Control.Plus (empty)
import Data.Array as Array
import Data.List (List(..), (:))
import Data.Maybe (Maybe, fromMaybe')
import Sexpze.Data.Sexp (Sexp(..))
import Sexpze.Utility (bug)

data Cursor
  = PointCursor Point
  | SpanCursor Span
  | SelectCursor Select

-- | A `Point` is a position between two elements of a `Group`.
data Point = Point (List Int) Int

topPoint :: Point
topPoint = Point mempty 0

consPoint :: Int -> Point -> Point
consPoint i (Point is j) = Point (i : is) j

movePoint_left :: forall a. Sexp a -> Point -> Maybe Point
movePoint_left (Atom _) _ = empty
movePoint_left (Group xs) (Point Nil j) =
  if not (0 <= j && j <= Array.length xs) then bug "invalid Point tip"
  else if j == 0 then empty
  else pure (Point Nil (j - 1))

movePoint_left (Group xs) (Point (i : is) j) =
  let
    x = xs Array.!! i # fromMaybe' \_ -> bug "invalid Point step"
  in
    consPoint i <$> movePoint_left x (Point is j)

movePoint_right :: forall a. Sexp a -> Point -> Maybe Point
movePoint_right (Atom _) _ = empty
movePoint_right (Group xs) (Point Nil j) =
  if not (0 <= j && j <= Array.length xs) then bug "invalid Point tip"
  else if j == Array.length xs then empty
  else pure (Point Nil (j + 1))

movePoint_right (Group xs) (Point (i : is) j) =
  let
    x = xs Array.!! i # fromMaybe' \_ -> bug "invalid Point step"
  in
    consPoint i <$> movePoint_right x (Point is j)

-- | A `Span` is a contiguous span between two `Point`s.
newtype Span = Span { p0 :: Point, p1 :: Point }

-- | A `Select` is composed of two `Span`s, with balanced groupings.
data Select = Select { s1 :: Span, s2 :: Span }
