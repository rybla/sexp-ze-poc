-- | A cursor into a s-expression.
module Sexpze.Data.Sexp.Cursor where

import Prelude

import Control.Plus (empty)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.List (List(..), (:))
import Data.Maybe (Maybe, fromMaybe')
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Sexpze.Data.Sexp (Sexp, Sexp'(..))
import Sexpze.Utility (bug, todo)

--------------------------------------------------------------------------------
-- SexpKidIndex
--------------------------------------------------------------------------------

-- | A `SexpKidIndex` specifies a kid of a `Sexp`.
newtype SexpKidIndex = SexpKidIndex Int

derive newtype instance Show SexpKidIndex
derive newtype instance Eq SexpKidIndex
derive newtype instance Ord SexpKidIndex

atSexpKidIndex :: forall a. SexpKidIndex -> Sexp a -> (Sexp' a -> Sexp a) /\ Sexp' a
atSexpKidIndex (SexpKidIndex i) xs =
  Tuple
    (\x -> xs # Array.updateAt i x # fromMaybe' (\_ -> bug "atSexpKidIndex" "index out of bounds"))
    (xs Array.!! i # fromMaybe' (\_ -> bug "atSexpKidIndex" "index out of bounds"))

--------------------------------------------------------------------------------
-- SexpPointIndex
--------------------------------------------------------------------------------

-- | A `SexpPointIndex` specifies a point between two kids of a `Sexp`.
newtype SexpPointIndex = SexpPointIndex Int

derive newtype instance Show SexpPointIndex
derive newtype instance Eq SexpPointIndex
derive newtype instance Ord SexpPointIndex

atSexpPointIndex :: forall a. SexpPointIndex -> Sexp a -> (Sexp a -> Sexp a)
atSexpPointIndex = todo "" {}

atSexpPointIndexSpan :: forall a. SexpPointIndex -> SexpPointIndex -> Sexp a -> (Sexp a -> Sexp a) /\ Sexp a
atSexpPointIndexSpan (SexpPointIndex i1) (SexpPointIndex i2) xs =
  Tuple
    ( \ys ->
        let
          { before, after: after_ } = Array.splitAt i1 xs
          after = Array.drop (i2 - i1) after_
        in
          before <> ys <> after
    )
    (Array.slice i1 i2 xs)

--------------------------------------------------------------------------------
-- Path
--------------------------------------------------------------------------------

type Path = List SexpKidIndex

atPath :: forall a. Path -> Sexp a -> (Sexp a -> Sexp a) /\ Sexp a
atPath ph xs = case ph of
  Nil -> identity /\ xs
  i : ph' -> case xs # atSexpKidIndex i of
    _ /\ Atom _ -> bug "atPath" "Path into Atom"
    w_i /\ Group xs_i ->
      lmap ((w_i <<< Group) <<< _) $
        atPath ph' xs_i

--------------------------------------------------------------------------------
-- Point
--------------------------------------------------------------------------------

-- | A `Point` is a `Path` to a sub-`Sexp` and then an index to a point between
-- | two of that sub-`Sexp`'s kids.
data Point = Point Path SexpPointIndex

unconsPoint :: Point -> Maybe (SexpKidIndex /\ Point)
unconsPoint (Point Nil _) = empty
unconsPoint (Point (i : ph) j) = pure (i /\ Point ph j)

atPoint :: forall a. Point -> Sexp a -> (Sexp a -> Sexp a)
atPoint (Point ph i) xs =
  let
    w_ph /\ xs_ph = atPath ph xs
  in
    w_ph <<< atSexpPointIndex i xs_ph

--------------------------------------------------------------------------------
-- Span
--------------------------------------------------------------------------------

data SpanCursor = SpanCursor Path SexpPointIndex SexpPointIndex

leftEndpoint_SpanCursor :: SpanCursor -> Point
leftEndpoint_SpanCursor (SpanCursor p i1 _) = Point p i1

rightEndpoint_SpanCursor :: SpanCursor -> Point
rightEndpoint_SpanCursor (SpanCursor p _ i2) = Point p i2

-- type Span a = Sexp a

atSpanCursor :: forall a. SpanCursor -> Sexp a -> (Sexp a -> Sexp a) /\ Sexp a
atSpanCursor (SpanCursor ph p1 p2) xs =
  let
    w_ph /\ xs_ph = atPath ph xs
  in
    lmap (w_ph <<< _) $
      atSexpPointIndexSpan p1 p2 xs_ph

--------------------------------------------------------------------------------
-- Zipper
--------------------------------------------------------------------------------

data ZipperCursor = ZipperCursor SpanCursor SpanCursor

-- | The `Point` is at the hole of the zipper.
data Zipper a = Zipper (Sexp a) Point

atZipperCursor :: forall a. ZipperCursor -> Sexp a -> ((Sexp a -> Sexp a) -> Sexp a) /\ Zipper a
atZipperCursor (ZipperCursor s1 s2) xs =
  let
    w1 /\ xs1 = atSpanCursor s1 xs
    _w2 /\ xs2 = atSpanCursor s2 xs1
  in
    Tuple
      (w1 <<< (_ $ xs2))
      (Zipper xs1 (leftEndpoint_SpanCursor s2))
