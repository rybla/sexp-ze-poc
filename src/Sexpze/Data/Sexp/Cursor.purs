-- | Cursors into `Sexp`s.
module Sexpze.Data.Sexp.Cursor where

import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/), Either4, Either5, in1, in2, in3, in4, in5, (\/))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (fromMaybe')
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Sexpze.Data.Sexp (Sexp'(..), Sexp)
import Sexpze.Utility (bug, todo)

--------------------------------------------------------------------------------
-- tmp
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- SexpKidIndex
--------------------------------------------------------------------------------

-- | A `SexpKidIndex` specifies a kid of a `Sexp`.
newtype SexpKidIndex = SexpKidIndex Int

derive instance Newtype SexpKidIndex _
derive newtype instance Show SexpKidIndex
derive newtype instance Eq SexpKidIndex
derive newtype instance Ord SexpKidIndex

atSexpKidIndex :: forall n a. SexpKidIndex -> Sexp n a -> (Sexp' n a -> Sexp n a) /\ Sexp' n a
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

atSexpPointIndex :: forall n a. SexpPointIndex -> Sexp n a -> (Sexp n a -> Sexp n a)
atSexpPointIndex (SexpPointIndex i) xs =
  let
    { before, after } = Array.splitAt i xs
  in
    \ys -> before <> ys <> after

atSexpPointIndexSpan :: forall n a. SexpPointIndex -> SexpPointIndex -> Sexp n a -> (Sexp n a -> Sexp n a) /\ Sexp n a
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

atPath :: forall n a. Path -> Sexp n a -> (Sexp n a -> Sexp n a) /\ Sexp n a
atPath ph xs = case ph of
  Nil -> identity /\ xs
  i : ph' -> case xs # atSexpKidIndex i of
    _ /\ Atom _ -> bug "atPath" "Path into Atom"
    w_i /\ Group n xs_i ->
      lmap ((w_i <<< Group n) <<< _) $
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

unconsPoint :: Point -> (SexpKidIndex /\ Point) \/ SexpPointIndex
unconsPoint (Point (i : ph) j) = Left (i /\ Point ph j)
unconsPoint (Point Nil j) = Right j

atPoint :: forall n a. Point -> Sexp n a -> (Sexp n a -> Sexp n a)
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

inbetweenPoints :: forall a. Array a -> Array SexpPointIndex
inbetweenPoints xs = Array.range 0 (Array.length xs) <#> wrap

foldSexpWithInbetweenPoints
  :: forall n a r
   . { list :: Array Point -> Array r -> r
     , group :: Path -> SexpKidIndex -> n -> r -> r
     , atom :: Path -> SexpKidIndex -> a -> r
     }
  -> Path
  -> Sexp n a
  -> r
foldSexpWithInbetweenPoints on = go1
  where
  go1 :: Path -> Sexp n a -> r
  go1 ph xs = on.list (xs # inbetweenPoints # map (Point ph)) (xs # Array.mapWithIndex (\i -> go2 ph (wrap i)))

  go2 :: Path -> SexpKidIndex -> Sexp' n a -> r
  go2 ph i (Atom a) = on.atom ph i a
  go2 ph i (Group n xs) = let xs' = go1 (ph `List.snoc` i) xs in on.group ph i n xs'

-- mapSexpWithInbetweenPoints
--   :: forall n n' a a'
--    . (Array Point -> Sexp n' a' -> Sexp n' a')
--   -> (Path -> SexpKidIndex -> n -> Sexp n' a' -> n')
--   -> (Path -> SexpKidIndex -> a -> a')
--   -> Path
--   -> Sexp n a
--   -> Sexp n' a'
-- mapSexpWithInbetweenPoints onList onGroup onAtom = go1
--   where
--   go1 :: Path -> Sexp n a -> Sexp n' a'
--   go1 ph xs = onList (xs # inbetweenPoints # map (Point ph)) (xs # Array.mapWithIndex (\i -> go2 ph (wrap i)))

--   go2 :: Path -> SexpKidIndex -> Sexp' n a -> Sexp' n' a'
--   go2 ph i (Atom a) = Atom (onAtom ph i a)
--   go2 ph i (Group n xs) = let xs' = go1 (ph `List.snoc` i) xs in Group (onGroup ph i n xs') xs'

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

unconsSpanCursor :: SpanCursor -> Either (SexpPointIndex /\ SexpPointIndex) (SexpKidIndex /\ SpanCursor)
unconsSpanCursor (SpanCursor ph j1 j2) = case ph of
  Nil -> Left (j1 /\ j2)
  i : ph' -> Right (i /\ SpanCursor ph' j1 j2)

-- type Span a = Sexp n a

atSpanCursor :: forall n a. SpanCursor -> Sexp n a -> (Sexp n a -> Sexp n a) /\ Sexp n a
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
data Zipper n a = Zipper (Sexp n a) Point

derive instance Generic (Zipper n a) _

instance (Show n, Show a) => Show (Zipper n a) where
  show x = genericShow x

instance (Eq n, Eq a) => Eq (Zipper n a) where
  eq x = genericEq x

type ZipperOrSpanCursor = ZipperCursor \/ SpanCursor

unconsZipperCursor
  :: ZipperOrSpanCursor
  -> Either5
       (SexpKidIndex /\ ZipperOrSpanCursor)
       ((SexpPointIndex /\ SexpPointIndex) /\ SexpKidIndex /\ ZipperOrSpanCursor)
       ((SexpPointIndex /\ SexpPointIndex) /\ (SexpPointIndex /\ SexpPointIndex))
       (SexpKidIndex /\ ZipperOrSpanCursor)
       (SexpPointIndex /\ SexpPointIndex)
unconsZipperCursor (Left (ZipperCursor s1 s2)) = case unconsSpanCursor s1 of
  Right (i /\ s1') -> in1 (i /\ Left (ZipperCursor s1' s2))
  Left (j1 /\ j2) -> case unconsSpanCursor s2 of
    Right (i /\ s2') -> in2 ((j1 /\ j2) /\ i /\ Right s2')
    Left (j1' /\ j2') -> in3 ((j1 /\ j2) /\ (j1' /\ j2'))
unconsZipperCursor (Right s) = case unconsSpanCursor s of
  Right (i /\ s') -> in4 (i /\ Right s')
  Left (j1 /\ j2) -> in5 (j1 /\ j2)

atZipperCursor :: forall n a. ZipperCursor -> Sexp n a -> ((Sexp n a -> Sexp n a) -> Sexp n a) /\ Zipper n a
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
  | InjectSpanCursor SpanCursor SpanHandle
  | InjectZipperCursor ZipperCursor ZipperHandle

derive instance Generic Cursor _

instance Show Cursor where
  show x = genericShow x

instance Eq Cursor where
  eq x = genericEq x

--------------------------------------------------------------------------------
-- pretty
--------------------------------------------------------------------------------

prettySexp :: forall n a. Show a => Sexp n a -> String
prettySexp xs = Array.intercalate " " $ map prettySexp' xs

prettySexp' :: forall n a. Show a => Sexp' n a -> String
prettySexp' (Atom a) = show a
prettySexp' (Group _ xs') = "(" <> prettySexp xs' <> ")"

prettySexpWithCursor :: forall n a. Show a => Cursor -> Sexp n a -> String
prettySexpWithCursor (InjectPoint p) xs = prettySexpWithPoint p xs
prettySexpWithCursor (InjectSpanCursor s _) xs = prettySexpWithSpanCursor s xs
prettySexpWithCursor (InjectZipperCursor z _) xs = prettySexpWithZipperCursor z xs

prettySexpWithPoint :: forall n a. Show a => Point -> Sexp n a -> String
prettySexpWithPoint p xs = case unconsPoint p of
  Left (i /\ p') ->
    xs
      # mapWithSexpPointIndex (\_ -> " ") (\i' x -> if i == i' then prettySexp'WithPoint p' x else prettySexp' x)
      # Array.fold
  Right j ->
    xs
      # mapWithSexpPointIndex (\j' -> if j == j' then "|" else " ") (const prettySexp')
      # Array.fold

prettySexp'WithPoint :: forall n a. Show a => Point -> Sexp' n a -> String
prettySexp'WithPoint (Point Nil _) x = prettySexp' x
prettySexp'WithPoint (Point (_ : _) _) (Atom _) = bug "prettySexp'WithPoint" "Path into Atom"
prettySexp'WithPoint p (Group _ xs') = "(" <> prettySexpWithPoint p xs' <> ")"

prettySexpWithSpanCursor :: forall n a. Show a => SpanCursor -> Sexp n a -> String
prettySexpWithSpanCursor s xs = prettySexpWithSpanCursor_helper prettySexp s xs

-- | The first given function will handle the span itself.
prettySexpWithSpanCursor_helper :: forall n a. Show a => (Sexp n a -> String) -> SpanCursor -> Sexp n a -> String
prettySexpWithSpanCursor_helper f (SpanCursor Nil j1 j2) xs =
  let
    { before, after: xs' } = xs # Array.splitAt (unwrap j1)
    { before: middle, after } = xs' # Array.splitAt (unwrap j2 - unwrap j1)
  in
    (before # map prettySexp' # Array.intercalate " ")
      <> (if Array.length before == 0 then "" else " ")
      <> f middle
      <> (if Array.length after == 0 then "" else " ")
      <> (after # map prettySexp' # Array.intercalate " ")
prettySexpWithSpanCursor_helper f (SpanCursor (i : ph) j1 j2) xs =
  xs
    # mapWithSexpKidIndex (\i' -> if i == i' then prettySexp'WithSpan_helper f (SpanCursor ph j1 j2) else prettySexp')
    # Array.fold

prettySexp'WithSpan_helper :: forall n a. Show a => (Sexp n a -> String) -> SpanCursor -> Sexp' n a -> String
prettySexp'WithSpan_helper _ _ (Atom _) = bug "prettySexp'WithSpan_helper" "Path into Atom"
prettySexp'WithSpan_helper f s (Group _ xs) = "(" <> prettySexpWithSpanCursor_helper f s xs <> ")"

prettySexpWithZipperCursor :: forall n a. Show a => ZipperCursor -> Sexp n a -> String
prettySexpWithZipperCursor (ZipperCursor s1 s2) xs =
  prettySexpWithSpanCursor_helper (prettySexpWithSpanCursor s2) s1 xs

--------------------------------------------------------------------------------
-- dragFromPoint
--------------------------------------------------------------------------------

dragFromPoint :: forall n a. Point -> Point -> Sexp n a -> Cursor
dragFromPoint p1_top@(Point ph1_top _i1_top) p2_top@(Point ph2_top _i2_top) xs =
  let
    ph /\ _p1@(Point ph1 j1) /\ _p2@(Point ph2 j2) = commonPathOfPoints p1_top p2_top
    _wrap_ph /\ _xs_ph = atPath ph xs
  in
    case ph1 /\ ph2 of
      Nil /\ Nil | j1 == j2 -> InjectPoint $ p1_top
      Nil /\ Nil -> if j1 <= j2 then InjectSpanCursor (SpanCursor ph j1 j2) EndSpanHandle else InjectSpanCursor (SpanCursor ph j2 j1) StartSpanHandle
      -- p1 is above p2
      Nil /\ (i2' : _) ->
        let
          _wrap_ph2_top /\ xs_ph2_top = atPath ph2_top xs
        in

          if isSexpPointIndexBeforeSexpKidIndex j1 i2' then
            -- p1 before p2
            InjectZipperCursor
              ( ZipperCursor
                  (SpanCursor ph (SexpPointIndex (unwrap j1)) (SexpPointIndex (unwrap j1 + 1)))
                  (SpanCursor ph2 (SexpPointIndex (unwrap j2)) (SexpPointIndex (Array.length xs_ph2_top)))
              )
              InnerStartZipperHandle
          else
            -- p2 before p1
            InjectZipperCursor
              ( ZipperCursor
                  (SpanCursor ph (SexpPointIndex (unwrap j1)) (SexpPointIndex (unwrap j1 + 1)))
                  (SpanCursor ph2 (SexpPointIndex 0) (SexpPointIndex (unwrap j2)))
              )
              InnerEndZipperHandle
      -- p2 is above p1
      (i1' : _) /\ Nil ->
        let
          _wrap_ph1_top /\ xs_ph1_top = atPath ph1_top xs
        in
          if isSexpPointIndexBeforeSexpKidIndex j2 i1' then
            -- p2 before p1
            InjectZipperCursor
              ( ZipperCursor
                  (SpanCursor ph (SexpPointIndex (unwrap j2)) (SexpPointIndex (unwrap j2 + 1)))
                  (SpanCursor ph1 (SexpPointIndex (unwrap j1)) (SexpPointIndex (Array.length xs_ph1_top)))
              )
              OuterStartZipperHandle

          else
            -- p1 before p2
            InjectZipperCursor
              ( ZipperCursor
                  (SpanCursor ph (SexpPointIndex (unwrap j2)) (SexpPointIndex (unwrap j2 + 1)))
                  (SpanCursor ph1 (SexpPointIndex 0) (SexpPointIndex (unwrap j1)))
              )
              OuterEndZipperHandle
      -- span around the kids that contain the endpoints
      (i1' : _ph1') /\ (i2' : _ph2') ->
        if i1' <= i2' then
          InjectSpanCursor (SpanCursor ph (SexpPointIndex (unwrap i1')) (SexpPointIndex (unwrap i2' + 1))) StartSpanHandle
        else
          InjectSpanCursor (SpanCursor ph (SexpPointIndex (unwrap i2')) (SexpPointIndex (unwrap i1' + 1))) EndSpanHandle

--------------------------------------------------------------------------------
-- dragFromSpan
--------------------------------------------------------------------------------

data SpanHandle = StartSpanHandle | EndSpanHandle

derive instance Generic SpanHandle _

instance Show SpanHandle where
  show x = genericShow x

instance Eq SpanHandle where
  eq x = genericEq x

dragFromSpan :: SpanCursor -> SpanHandle -> Point -> Cursor
dragFromSpan _s = todo "dragFromSpan" {}

--------------------------------------------------------------------------------
-- dragFromZipper
--------------------------------------------------------------------------------

dragFromZipper :: ZipperCursor -> ZipperHandle -> Point -> Cursor
dragFromZipper _z = todo "dragFromZipper" {}

data ZipperHandle
  = OuterStartZipperHandle
  | OuterEndZipperHandle
  | InnerStartZipperHandle
  | InnerEndZipperHandle

derive instance Generic ZipperHandle _

instance Show ZipperHandle where
  show x = genericShow x

instance Eq ZipperHandle where
  eq x = genericEq x

--------------------------------------------------------------------------------
-- utilities
--------------------------------------------------------------------------------

order :: forall a. Ord a => a -> a -> a /\ a
order x y = if x <= y then x /\ y else y /\ x

mapWithSexpPointIndex :: forall a b. (SexpPointIndex -> b) -> (SexpKidIndex -> a -> b) -> Array a -> Array b
mapWithSexpPointIndex f g =
  Array.fold
    <<< Array.cons [ f (wrap 0) ]
    <<< Array.mapWithIndex (\i a -> [ g (wrap i) a, f (wrap (i + 1)) ])

mapWithSexpKidIndex :: forall a b. (SexpKidIndex -> a -> b) -> Array a -> Array b
mapWithSexpKidIndex f = Array.mapWithIndex (f <<< wrap)
