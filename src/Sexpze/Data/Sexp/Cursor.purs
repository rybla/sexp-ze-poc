module Sexpze.Data.Sexp.Cursor where

import Prelude

import Control.Plus (empty)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/), Either5, in1, in2, in3, in4, in5)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (class Newtype, over2, unwrap, wrap)
import Data.Ordering (invert)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Sexpze.Data.Sexp (Sexp(..), Sexp'(..))
import Sexpze.Utility (bug, todo)

--------------------------------------------------------------------------------
-- KidIndex
--------------------------------------------------------------------------------

newtype KidIndex = KidIndex Int

derive instance Newtype KidIndex _
derive newtype instance Show KidIndex
derive newtype instance Eq KidIndex
derive newtype instance Ord KidIndex

modifySexpAt :: forall n a. KidIndex -> (Sexp' n a -> Sexp' n a) -> Sexp n a -> Sexp n a
modifySexpAt (KidIndex i) f (Sexp n e's) = Sexp n (e's # Array.modifyAt i f # fromMaybe' (\_ -> bug "modifySexpAt" "KidIndex out of bounds"))

modifySpanAt :: forall n a. KidIndex -> (Sexp' n a -> Sexp' n a) -> Span n a -> Span n a
modifySpanAt (KidIndex i) f (Span e's) = Span (e's # Array.modifyAt i f # fromMaybe' (\_ -> bug "modifySpanAt" "KidIndex out of bounds"))

getKid :: forall n a. KidIndex -> Span n a -> Sexp' n a
getKid (KidIndex i) (Span e's) = e's Array.!! i # fromMaybe' (\_ -> bug "getSexpKid" "KidIndex out of bounds")

atKidIndex :: forall n a. KidIndex -> Span n a -> (Sexp' n a -> Span n a) /\ Sexp' n a
atKidIndex i e =
  Tuple
    (\e' -> e # modifySpanAt i (const e'))
    (e # getKid i)

--------------------------------------------------------------------------------
-- PointIndex
--------------------------------------------------------------------------------

newtype PointIndex = PointIndex Int

derive instance Newtype PointIndex _
derive newtype instance Show PointIndex
derive newtype instance Eq PointIndex
derive newtype instance Ord PointIndex

pointIndexRightBeforeKidIndex :: KidIndex -> PointIndex
pointIndexRightBeforeKidIndex i = wrap (unwrap i)

pointIndexRightAfterKidIndex :: KidIndex -> PointIndex
pointIndexRightAfterKidIndex i = wrap (unwrap i + 1)

-- | true ==> LT; false ==> GT
compareKidIndexToPointIndex' :: KidIndex -> PointIndex -> Boolean
compareKidIndexToPointIndex' i j = unwrap i < unwrap j

compareKidIndexToPointIndex :: KidIndex -> PointIndex -> Ordering
compareKidIndexToPointIndex i j = if compareKidIndexToPointIndex' i j then LT else GT

-- | true ==> LT; false ==> GT
comparePointIndexToKidIndex' :: PointIndex -> KidIndex -> Boolean
comparePointIndexToKidIndex' j i = compareKidIndexToPointIndex' i j # not

comparePointIndexToKidIndex :: PointIndex -> KidIndex -> Ordering
comparePointIndexToKidIndex j i = compareKidIndexToPointIndex i j # invert

atPointIndex :: forall n a. PointIndex -> Span n a -> (Span n a -> Span n a)
atPointIndex i (Span es) (Span es') =
  let
    { before, after } = es # Array.splitAt (unwrap i)
  in
    Span (before <> es' <> after)

mapWithPointIndex :: forall a r. (PointIndex -> r) -> (KidIndex -> a -> r) -> Array a -> Array r
mapWithPointIndex f_point f_kid xs =
  [ [ f_point (wrap 0) ] ] <> (xs # Array.mapWithIndex \i x -> [ f_kid (wrap i) x, f_point (wrap (i + 1)) ])
    # Array.fold

--------------------------------------------------------------------------------
-- Path
--------------------------------------------------------------------------------

newtype Path = Path (List KidIndex)

derive instance Newtype Path _
derive newtype instance Show Path
derive newtype instance Eq Path

instance Semigroup Path where
  append (Path ph) (Path Nil) = Path ph
  append (Path Nil) (Path ph) = Path ph
  append (Path (i1 : Nil)) (Path (i2 : ph2)) = Path $ over2 KidIndex add i1 i2 : ph2
  append (Path (i1 : ph1)) (Path ph2) = consPath i1 (Path ph1 <> Path ph2)

instance Monoid Path where
  mempty = Path Nil

unconsPath :: Path -> Maybe (KidIndex /\ Path)
unconsPath (Path Nil) = empty
unconsPath (Path (i : is)) = pure (i /\ Path is)

consPath :: KidIndex -> Path -> Path
consPath i (Path is) = Path (i : is)

snocPath :: Path -> KidIndex -> Path
snocPath (Path is) i = Path (is `List.snoc` i)

atPath :: forall n a. Path -> Span n a -> (Span n a -> Span n a) /\ Span n a
atPath ph e = case unconsPath ph of
  Nothing -> identity /\ e
  Just (i /\ ph') -> case e # atKidIndex i of
    w_i /\ e_i ->
      let
        w_e'_i /\ e'_i = inSexp' e_i
      in
        lmap ((w_i <<< w_e'_i) <<< _) $ atPath ph' e'_i

commonPath :: Path -> Path -> Path -> Path /\ (Path /\ Path)
commonPath ph ph1 ph2 = case unconsPath ph1 /\ unconsPath ph2 of
  Just (i1 /\ ph1') /\ Just (i2 /\ ph2') | i1 == i2 -> commonPath (ph `snocPath` i1) ph1' ph2'
  _ -> ph /\ (ph1 /\ ph2)

commonPathOfPointCursors :: PointCursor -> PointCursor -> Path /\ (PointCursor /\ PointCursor)
commonPathOfPointCursors (PointCursor ph1 j1) (PointCursor ph2 j2) =
  let
    ph /\ ph1' /\ ph2' = commonPath mempty ph1 ph2
  in
    ph /\ PointCursor ph1' j1 /\ PointCursor ph2' j2

--------------------------------------------------------------------------------
-- PointCursor
--------------------------------------------------------------------------------

data PointCursor = PointCursor Path PointIndex

derive instance Generic PointCursor _

instance Show PointCursor where
  show x = genericShow x

instance Eq PointCursor where
  eq x = genericEq x

instance Ord PointCursor where
  compare p1 p2 =
    let
      p1' /\ p2' = commonPathOfPointCursors p1 p2 # snd
    in
      case unconsPointCursor p1' /\ unconsPointCursor p2' of
        Left (i1 /\ _) /\ Left (i2 /\ _) -> compare i1 i2
        Left (i1 /\ _) /\ Right j2 -> compareKidIndexToPointIndex i1 j2
        Right j1 /\ Left (i2 /\ _) -> comparePointIndexToKidIndex j1 i2
        Right j1 /\ Right j2 -> compare j1 j2

zeroPointCursor :: PointCursor
zeroPointCursor = PointCursor mempty (wrap 0)

consPointCursor :: KidIndex -> PointCursor -> PointCursor
consPointCursor i (PointCursor ph j) = PointCursor (consPath i ph) j

unconsPointCursor :: PointCursor -> (KidIndex /\ PointCursor) \/ PointIndex
unconsPointCursor (PointCursor ph j) = case unconsPath ph of
  Just (i /\ ph') -> Left (i /\ PointCursor ph' j)
  Nothing -> Right j

atPointCursor :: forall n a. (Span n a -> Span n a) -> PointCursor -> Span n a -> (Span n a -> Span n a)
atPointCursor w p e = case unconsPointCursor p of
  Left (i /\ p') ->
    let
      w' /\ e' = e # atKidIndex i
      w'' /\ e'' = e' # inSexp'
    in
      atPointCursor (w <<< w' <<< w'') p' e''
  Right j ->
    let
      w' = e # atPointIndex j
    in
      w <<< w'

comparePointCursors :: PointCursor -> PointCursor -> Ordering
comparePointCursors (PointCursor ph1 j1) (PointCursor ph2 j2) =
  let
    _ /\ (ph1' /\ ph2') = commonPath mempty ph1 ph2
  in
    case unconsPath ph1' /\ unconsPath ph2' of
      Nothing /\ Nothing -> compare j1 j2
      Just (i1 /\ _) /\ Nothing -> compareKidIndexToPointIndex i1 j2
      Nothing /\ Just (i2 /\ _) -> comparePointIndexToKidIndex j1 i2
      Just (i1 /\ _) /\ Just (i2 /\ _) -> compare i1 i2

--------------------------------------------------------------------------------
-- PointDist
--------------------------------------------------------------------------------

newtype PointDist = PointDist Int

derive instance Newtype PointDist _

derive newtype instance Show PointDist
derive newtype instance Eq PointDist
derive newtype instance Semiring PointDist

newtype PointDistNeg = PointDistNeg Int

derive instance Newtype PointDistNeg _

derive newtype instance Show PointDistNeg
derive newtype instance Eq PointDistNeg
derive newtype instance Semiring PointDistNeg

getPointDistBetweenPointIndices :: PointIndex -> PointIndex -> PointDist
getPointDistBetweenPointIndices j1 j2 = wrap (unwrap j2 - unwrap j1)

shiftKidIndexByPointDist :: PointDist -> KidIndex -> KidIndex
shiftKidIndexByPointDist d i = wrap (unwrap i + unwrap d)

trimKidIndexByPointDist :: PointDist -> KidIndex -> KidIndex
trimKidIndexByPointDist d i = wrap (unwrap i - unwrap d)

shiftPointCursorByPointDist :: PointDist -> PointCursor -> PointCursor
shiftPointCursorByPointDist d (PointCursor ph j) = PointCursor ph (wrap (unwrap j + unwrap d))

shiftPointCursorByPointDistNeg :: PointDistNeg -> PointCursor -> PointCursor
shiftPointCursorByPointDistNeg d (PointCursor ph j) = PointCursor ph (wrap (unwrap j - unwrap d))

shiftPointIndexByPointDist :: PointDist -> PointIndex -> PointIndex
shiftPointIndexByPointDist d j = wrap (unwrap j + unwrap d)

shiftPointIndexByPointDistNeg :: PointDistNeg -> PointIndex -> PointIndex
shiftPointIndexByPointDistNeg d j = wrap (unwrap j - unwrap d)

getOffsetPointDist :: forall n a. PointIndex -> Span n a -> PointDist
getOffsetPointDist i _ = wrap (unwrap i)

getOffsetPointDistNeg :: forall n a. PointIndex -> Span n a -> PointDistNeg
getOffsetPointDistNeg i (Span es) = wrap (Array.length es - unwrap i)

getPointIndexFromPointDist :: forall n a. PointDist -> Span n a -> PointIndex
getPointIndexFromPointDist d _ = wrap (unwrap d)

getPointIndexFromPointDistNeg :: forall n a. PointDistNeg -> Span n a -> PointIndex
getPointIndexFromPointDistNeg d (Span es) = wrap (Array.length es - unwrap d)

getSpanCursorBetweenPointIndices :: forall n a. Path -> PointIndex -> PointIndex -> Span n a -> SpanCursor
getSpanCursorBetweenPointIndices ph j1 j2 s = SpanCursor ph (getOffsetPointDist j1 s) (getOffsetPointDistNeg j2 s)

pointDistBetweenPointIndices :: PointIndex -> PointIndex -> PointDist
pointDistBetweenPointIndices j1 j2 = wrap (unwrap j2 - unwrap j1)

--------------------------------------------------------------------------------
-- SpanCursor
--------------------------------------------------------------------------------

data SpanCursor = SpanCursor Path PointDist PointDistNeg

derive instance Generic SpanCursor _

instance Show SpanCursor where
  show x = genericShow x

instance Eq SpanCursor where
  eq x = genericEq x

trimKidIndexIntoSpanCursor :: SpanCursor -> KidIndex -> KidIndex
trimKidIndexIntoSpanCursor (SpanCursor _ d1 _) = trimKidIndexByPointDist d1

instance Semigroup SpanCursor where
  append (SpanCursor ph_outer d1_outer d2_outer) (SpanCursor ph_inner d1_inner d2_inner) =
    case unconsPath ph_inner of
      Nothing -> SpanCursor ph_outer (d1_outer + d1_inner) (d2_outer + d2_inner)
      Just (i_inner /\ ph'_inner) -> SpanCursor (ph_outer <> ((d1_outer `shiftKidIndexByPointDist` i_inner) `consPath` ph'_inner)) d1_inner d2_inner

getSpanCursorHandle :: forall n a. SpanCursor -> SpanHandle -> Span n a -> PointCursor
getSpanCursorHandle (SpanCursor ph d _) Start e = PointCursor ph (d `shiftPointIndexByPointDist` firstPointIndexOfSpan e)
getSpanCursorHandle (SpanCursor ph _ d) End e = PointCursor ph (d `shiftPointIndexByPointDistNeg` lastPointIndexOfSpan e)

newtype Span n a = Span (Array (Sexp' n a))

derive instance Newtype (Span n a) _
derive newtype instance (Show n, Show a) => Show (Span n a)
derive newtype instance (Eq n, Eq a) => Eq (Span n a)

data SpanHandle = Start | End

derive instance Generic SpanHandle _

instance Show SpanHandle where
  show x = genericShow x

instance Eq SpanHandle where
  eq x = genericEq x

toSpan :: forall n a. Sexp n a -> Span n a
toSpan (Sexp _ es) = Span es

fromSpan :: forall n a. n -> Span n a -> Sexp n a
fromSpan n (Span es) = (Sexp n es)

firstPointIndexOfSpan :: forall n a. Span n a -> PointIndex
firstPointIndexOfSpan _ = wrap 0

lastPointIndexOfSpan :: forall n a. Span n a -> PointIndex
lastPointIndexOfSpan (Span es) = wrap (Array.length es)

-- | safe constructor. checks:
-- |   - p1 <= p2
spanCursor :: Path -> PointDist -> PointDistNeg -> SpanCursor
spanCursor = SpanCursor -- TODO

unconsSpanCursor
  :: SpanCursor
  -> Either
       (KidIndex /\ SpanCursor)
       (PointDist /\ PointDistNeg)
unconsSpanCursor (SpanCursor ph j1 j2) = case unconsPath ph of
  Just (i /\ ph') -> Left (i /\ SpanCursor ph' j1 j2)
  Nothing -> Right (j1 /\ j2)

atPointIndexSpan :: forall n a. PointIndex -> PointIndex -> Sexp n a -> (Span n a -> Sexp n a) /\ Span n a
atPointIndexSpan i1 i2 (Sexp n es) =
  let
    { before, after: after_ } = es # Array.splitAt (unwrap i1)
    { before: middle, after } = after_ # Array.splitAt (unwrap i2 - unwrap i1)
  in
    Tuple
      (\(Span es') -> Sexp n (before <> es' <> after))
      (Span middle)

atSpanPointIndexSpan :: forall n a. PointDist -> PointDistNeg -> Span n a -> (Span n a -> Span n a) /\ Span n a
atSpanPointIndexSpan d1 d2 (Span es) =
  let
    i0 = 0
    i1 = unwrap d1
    i2 = Array.length es - unwrap d2
    i3 = Array.length es
    before = es # Array.slice i0 i1
    middle = es # Array.slice i1 i2
    after = es # Array.slice i2 i3
  in
    Tuple
      (\(Span es') -> Span (before <> es' <> after))
      (Span middle)

atSpanCursor :: forall n a. SpanCursor -> Span n a -> (Span n a -> Span n a) /\ Span n a
atSpanCursor (SpanCursor ph j1 j2) =
  atPath ph >>> \(w /\ e') ->
    lmap (w <<< _)
      $ atSpanPointIndexSpan j1 j2 e'

--------------------------------------------------------------------------------
-- ZipperCursor
--------------------------------------------------------------------------------

data ZipperCursor = ZipperCursor SpanCursor SpanCursor

derive instance Generic ZipperCursor _

instance Show ZipperCursor where
  show x = genericShow x

instance Eq ZipperCursor where
  eq x = genericEq x

-- | safe constructor. checks:
-- |   - s2 is non-empty
zipperCursor :: SpanCursor -> SpanCursor -> ZipperCursor
zipperCursor = ZipperCursor -- TODO

getZipperCursorHandle :: forall n a. ZipperCursor -> ZipperHandle -> Span n a -> PointCursor
getZipperCursorHandle (ZipperCursor s _) (Outer h) = getSpanCursorHandle s h
getZipperCursorHandle (ZipperCursor s_outer s_inner) (Inner h) = getSpanCursorHandle (s_outer <> s_inner) h

data Zipper n a = Zipper (Span n a) PointCursor

derive instance Generic (Zipper n a) _

instance (Show n, Show a) => Show (Zipper n a) where
  show x = genericShow x

instance (Eq n, Eq a) => Eq (Zipper n a) where
  eq x = genericEq x

emptyZipper :: forall n a. Zipper n a
emptyZipper = Zipper (Span []) zeroPointCursor

inZipper :: forall n a. Zipper n a -> (Span n a -> Span n a)
inZipper (Zipper e p) = atPointCursor identity p e

unconsZipperCursor
  :: ZipperCursor \/ SpanCursor
  -> Either5
       -- midst of outer path
       (KidIndex /\ ZipperCursor)
       -- end of outer path; midst of inner path
       ((PointDist /\ PointDistNeg) /\ KidIndex /\ SpanCursor)
       -- end of outer path; end of inner path since it's empty
       ((PointDist /\ PointDistNeg) /\ (PointDist /\ PointDistNeg))
       -- midst of inner path
       (KidIndex /\ SpanCursor)
       -- end of inner path
       (PointDist /\ PointDistNeg)
unconsZipperCursor (Left (ZipperCursor s1 s2)) | Left (i1 /\ s1') <- unconsSpanCursor s1 = in1 (i1 /\ ZipperCursor s1' s2)
unconsZipperCursor (Left (ZipperCursor s1 s2)) | Right (j1_start /\ j1_end) <- unconsSpanCursor s1, Left (i2 /\ s2') <- unconsSpanCursor s2 = in2 ((j1_start /\ j1_end) /\ i2 /\ s2')
unconsZipperCursor (Left (ZipperCursor s1 s2)) | Right (j1_start /\ j1_end) <- unconsSpanCursor s1, Right (j2_start /\ j2_end) <- unconsSpanCursor s2 = in3 ((j1_start /\ j1_end) /\ (j2_start /\ j2_end))
unconsZipperCursor (Right s) | Left (i /\ s') <- unconsSpanCursor s = in4 (i /\ s')
unconsZipperCursor (Right s) | Right (j1 /\ j2) <- unconsSpanCursor s = in5 (j1 /\ j2)
unconsZipperCursor _ = bug "unconsZipperCursor" "impossible"

atZipperCursor :: forall n a. ZipperCursor -> Span n a -> ((Span n a -> Span n a) -> Span n a) /\ Zipper n a
atZipperCursor (ZipperCursor s_outer s_inner@(SpanCursor ph_inner d1_inner _)) e =
  let
    w1 /\ e1 = e # atSpanCursor s_outer
    _w2 /\ e2 = e1 # atSpanCursor s_inner
  in
    Tuple
      (w1 <<< (_ $ e2))
      (Zipper e2 (PointCursor ph_inner (wrap 0) # shiftPointCursorByPointDist d1_inner))

insertAtZipperCursor :: forall n a. ZipperCursor -> Zipper n a -> Span n a -> Span n a
insertAtZipperCursor c z = atZipperCursor c >>> fst >>> (_ $ inZipper z)

deleteAtZipperCursor :: forall n a. ZipperCursor -> Span n a -> Span n a
deleteAtZipperCursor c = insertAtZipperCursor c emptyZipper

--------------------------------------------------------------------------------
-- Cursor
--------------------------------------------------------------------------------

data Cursor = Cursor ZipperCursor ZipperHandle

derive instance Generic Cursor _

instance Show Cursor where
  show x = genericShow x

instance Eq Cursor where
  eq x = genericEq x

data ZipperHandle = Inner SpanHandle | Outer SpanHandle

derive instance Generic ZipperHandle _

instance Show ZipperHandle where
  show x = genericShow x

instance Eq ZipperHandle where
  eq x = genericEq x

getCursorHandle :: forall n a. Cursor -> Span n a -> PointCursor
getCursorHandle (Cursor z h) = getZipperCursorHandle z h

fromPointCursorToCursor :: forall n a. PointCursor -> Span n a -> Cursor
fromPointCursorToCursor p e = Cursor (fromPointCursorToZipperCursor p e) (Inner Start)

fromPointCursorToZipperCursor :: forall n a. PointCursor -> Span n a -> ZipperCursor
fromPointCursorToZipperCursor p e = let s = fromPointCursorToZeroWidthSpanCursor p e in ZipperCursor s (SpanCursor mempty zero zero)

fromPointCursorToZeroWidthSpanCursor :: forall n a. PointCursor -> Span n a -> SpanCursor
fromPointCursorToZeroWidthSpanCursor (PointCursor ph i) e =
  let
    _ /\ e_inner = e # atPath ph
  in
    SpanCursor ph (getOffsetPointDist i e_inner) (getOffsetPointDistNeg i e_inner)

insertAtCursor :: forall n a. Zipper n a -> Cursor -> Span n a -> Cursor /\ Span n a
insertAtCursor z@(Zipper _ p_inner) (Cursor c@(ZipperCursor s_outer _s_inner) h) e =
  let
    e' = e # insertAtZipperCursor c z
    _ /\ e_middle = e' # atSpanCursor s_outer
    s_middle = fromPointCursorToZeroWidthSpanCursor p_inner e_middle
    c' = Cursor (ZipperCursor (s_outer <> s_middle) (SpanCursor mempty zero zero)) h
  in
    c' /\ e'

-- (Cursor (ZipperCursor s_outer (s_inner <> fromPointCursorToZeroWidthSpanCursor p_inner e_middle)) h)

deleteAtCursor :: forall n a. Cursor -> Span n a -> Cursor /\ Span n a
deleteAtCursor = insertAtCursor emptyZipper

--------------------------------------------------------------------------------
-- misc
--------------------------------------------------------------------------------

inSexp' :: forall n a. Sexp' n a -> (Span n a -> Sexp' n a) /\ Span n a
inSexp' (Atom _) = bug "inSexp'" "can't go in an Atom"
inSexp' (Group (Sexp n e)) = (Group <<< fromSpan n) /\ Span e

