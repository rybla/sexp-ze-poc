-- | Cursors into `Sexp`s.
module Sexpze.Data.Sexp.Cursor where

import Prelude

import Control.Plus (empty)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Maybe (Maybe, fromMaybe')
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Sexpze.Data.Sexp (Sexp, Sexp'(..))
import Sexpze.Utility (bug, todo)

--------------------------------------------------------------------------------
-- SexpKidIndex
--------------------------------------------------------------------------------

-- | A `SexpKidIndex` specifies a kid of a `Sexp`.
newtype SexpKidIndex = SexpKidIndex Int

derive instance Newtype SexpKidIndex _
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

derive instance Newtype SexpPointIndex _
derive newtype instance Show SexpPointIndex
derive newtype instance Eq SexpPointIndex
derive newtype instance Ord SexpPointIndex

atSexpPointIndex :: forall a. SexpPointIndex -> Sexp a -> (Sexp a -> Sexp a)
atSexpPointIndex (SexpPointIndex i) xs =
  let
    { before, after } = Array.splitAt i xs
  in
    \ys -> before <> ys <> after

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

-- | true = LT, false = GT
isSexpPointIndexBeforeSexpKidIndex :: SexpPointIndex -> SexpKidIndex -> Boolean
isSexpPointIndexBeforeSexpKidIndex (SexpPointIndex i) (SexpKidIndex j) = i <= j

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

commonPathOfPoints :: Point -> Point -> Path /\ Point /\ Point
commonPathOfPoints (Point (i : ph1) j1) (Point (i_ : ph2) j2) | i == i_ =
  lmap (i : _) $
    commonPathOfPoints (Point ph1 j1) (Point ph2 j2)
commonPathOfPoints p1 p2 = Nil /\ p1 /\ p2

--------------------------------------------------------------------------------
-- Point
--------------------------------------------------------------------------------

-- | A `Point` is a `Path` to a sub-`Sexp` and then an index to a point between
-- | two of that sub-`Sexp`'s kids.
data Point = Point Path SexpPointIndex

derive instance Generic Point _

instance Show Point where
  show x = genericShow x

instance Eq Point where
  eq x = genericEq x

instance Ord Point where
  compare (Point Nil j1) (Point Nil j2) = compare j1 j2
  compare (Point Nil j1) (Point (i2 : _) _) = if isSexpPointIndexBeforeSexpKidIndex j1 i2 then LT else GT
  compare (Point (i1 : _) _) (Point Nil j2) = if isSexpPointIndexBeforeSexpKidIndex j2 i1 then GT else LT
  compare (Point (i1 : ph1) j1) (Point (i2 : ph2) j2) = case compare i1 i2 of
    LT -> LT
    EQ -> compare (Point ph1 j1) (Point ph2 j2)
    GT -> GT

unconsPoint :: Point -> Maybe (SexpKidIndex /\ Point)
unconsPoint (Point Nil _) = empty
unconsPoint (Point (i : ph) j) = pure (i /\ Point ph j)

atPoint :: forall a. Point -> Sexp a -> (Sexp a -> Sexp a)
atPoint (Point ph i) xs =
  let
    w_ph /\ xs_ph = atPath ph xs
  in
    w_ph <<< atSexpPointIndex i xs_ph

orderPoints :: Point -> Point -> Ordering /\ Point /\ Point
orderPoints p1 p2 =
  let
    o = compare p1 p2
  in
    o /\ case o of
      LT -> p1 /\ p2
      _ -> p2 /\ p1

--------------------------------------------------------------------------------
-- Span
--------------------------------------------------------------------------------

data SpanCursor = SpanCursor Path SexpPointIndex SexpPointIndex

derive instance Generic SpanCursor _

instance Show SpanCursor where
  show x = genericShow x

instance Eq SpanCursor where
  eq x = genericEq x

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

derive instance Generic ZipperCursor _

instance Show ZipperCursor where
  show x = genericShow x

instance Eq ZipperCursor where
  eq x = genericEq x

-- | The `Point` is at the hole of the zipper.
data Zipper a = Zipper (Sexp a) Point

derive instance Generic (Zipper a) _

instance Show a => Show (Zipper a) where
  show x = genericShow x

instance Eq a => Eq (Zipper a) where
  eq x = genericEq x

atZipperCursor :: forall a. ZipperCursor -> Sexp a -> ((Sexp a -> Sexp a) -> Sexp a) /\ Zipper a
atZipperCursor (ZipperCursor s1 s2) xs =
  let
    w1 /\ xs1 = atSpanCursor s1 xs
    _w2 /\ xs2 = atSpanCursor s2 xs1
  in
    Tuple
      (w1 <<< (_ $ xs2))
      (Zipper xs1 (leftEndpoint_SpanCursor s2))

--------------------------------------------------------------------------------
-- Cursor
--------------------------------------------------------------------------------

data Cursor
  = InjectPoint Point
  | InjectSpanCursor SpanCursor
  | InjectZipperCursor ZipperCursor

derive instance Generic Cursor _

instance Show Cursor where
  show x = genericShow x

instance Eq Cursor where
  eq x = genericEq x

--------------------------------------------------------------------------------
-- dragFromPoint
--------------------------------------------------------------------------------

dragFromPoint :: forall a. Point -> Point -> Sexp a -> Cursor
dragFromPoint p1_top@(Point ph1_top _i1_top) p2_top@(Point ph2_top _i2_top) xs =
  let
    ph /\ _p1@(Point ph1 j1) /\ _p2@(Point ph2 j2) = commonPathOfPoints p1_top p2_top
    _wrap_ph /\ _xs_ph = atPath ph xs
  in
    case ph1 /\ ph2 of
      -- ==> PointCursor
      Nil /\ Nil | j1 == j2 -> InjectPoint $ p1_top
      -- ==> SpanCursor
      Nil /\ Nil -> InjectSpanCursor $ SpanCursor ph j1 j2
      -- p1 is above p2
      -- ==> ZipperCursor
      Nil /\ (i2' : _) ->
        let
          _wrap_ph2_top /\ xs_ph2_top = atPath ph2_top xs
        in
          InjectZipperCursor $
            ZipperCursor
              (SpanCursor ph (SexpPointIndex (unwrap j2)) (SexpPointIndex (unwrap j2 + 1)))
              ( if isSexpPointIndexBeforeSexpKidIndex j1 i2' then
                  SpanCursor ph2 (SexpPointIndex (unwrap j1)) (SexpPointIndex (Array.length xs_ph2_top))
                else
                  SpanCursor ph2 (SexpPointIndex 0) (SexpPointIndex (unwrap j1))
              )
      -- p2 is above p1
      -- ==> ZipperCursor
      (i1' : _) /\ Nil ->
        let
          _wrap_ph2_top /\ xs_ph1_top = atPath ph1_top xs
        in
          InjectZipperCursor $
            ZipperCursor
              (SpanCursor ph (SexpPointIndex (unwrap j1)) (SexpPointIndex (unwrap j1 + 1)))
              ( if isSexpPointIndexBeforeSexpKidIndex j2 i1' then
                  SpanCursor ph1 (SexpPointIndex (unwrap j2)) (SexpPointIndex (Array.length xs_ph1_top))
                else
                  SpanCursor ph1 (SexpPointIndex 0) (SexpPointIndex (unwrap j2))
              )
      -- ==> Span
      -- span around the kids that contain the endpoints
      (i1'_ : _ph1') /\ (i2'_ : _ph2') ->
        let
          i1' /\ i2' = order i1'_ i2'_
        in
          InjectSpanCursor $ SpanCursor ph (SexpPointIndex (unwrap i1')) (SexpPointIndex (unwrap i2' + 1))

--------------------------------------------------------------------------------
-- dragFromZipper
--------------------------------------------------------------------------------

data SpanHandle = StartSpanHandle | EndSpanHandle

dragFromSpan :: SpanCursor -> SpanHandle -> Point -> Cursor
dragFromSpan _s = todo "dragFromSpan" {}

--------------------------------------------------------------------------------
-- dragFromZipper
--------------------------------------------------------------------------------

dragFromZipper :: ZipperCursor -> ZipperHandle -> Point -> Cursor
dragFromZipper _z = todo "dragFromZipper" {}

data ZipperHandle = StartZipperHandle | EndZipperHandle

--------------------------------------------------------------------------------
-- utilities
--------------------------------------------------------------------------------

order :: forall a. Ord a => a -> a -> a /\ a
order x y = if x <= y then x /\ y else y /\ x

