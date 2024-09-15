module Sexpze.Data.Sexp.Cursor where

import Prelude

import Control.Plus (empty)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (class Newtype, over2, unwrap)
import Data.Newtype as Newtype
import Data.Ordering (invert)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..), snd)
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

getSexpKid :: forall n a. KidIndex -> Sexp n a -> Sexp' n a
getSexpKid (KidIndex i) (Sexp _ e's) = e's Array.!! i # fromMaybe' (\_ -> bug "getSexpKid" "KidIndex out of bounds")

atKidIndex :: forall n a. KidIndex -> Sexp n a -> (Sexp' n a -> Sexp n a) /\ Sexp' n a
atKidIndex i e =
  Tuple
    (\e' -> e # modifySexpAt i (const e'))
    (e # getSexpKid i)

--------------------------------------------------------------------------------
-- PointIndex
--------------------------------------------------------------------------------

newtype PointIndex = PointIndex Int

derive instance Newtype PointIndex _
derive newtype instance Show PointIndex
derive newtype instance Eq PointIndex
derive newtype instance Ord PointIndex

compareKidIndexToPointIndex' :: KidIndex -> PointIndex -> Boolean
compareKidIndexToPointIndex' i j = unwrap i < unwrap j

compareKidIndexToPointIndex :: KidIndex -> PointIndex -> Ordering
compareKidIndexToPointIndex i j = if compareKidIndexToPointIndex' i j then LT else GT

comparePointIndexToKidIndex' :: PointIndex -> KidIndex -> Boolean
comparePointIndexToKidIndex' j i = compareKidIndexToPointIndex' i j # not

comparePointIndexToKidIndex :: PointIndex -> KidIndex -> Ordering
comparePointIndexToKidIndex j i = compareKidIndexToPointIndex i j # invert

atPointIndex :: forall n a. PointIndex -> Sexp n a -> (Sexp n a -> Sexp n a)
atPointIndex i (Sexp n es) (Sexp _ es') =
  let
    { before, after } = es # Array.splitAt (unwrap i)
  in
    Sexp n (before <> es' <> after)

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

atPath :: forall n a. Path -> Sexp n a -> (Sexp n a -> Sexp n a) /\ Sexp n a
atPath ph e = case unconsPath ph of
  Nothing -> identity /\ e
  Just (i /\ ph') -> case e # atKidIndex i of
    _ /\ Atom _ -> bug "atPath" "Path into Atom"
    wrap_i /\ Group e_i -> lmap ((wrap_i <<< Group) <<< _) $ atPath ph' e_i

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

consPointCursor :: KidIndex -> PointCursor -> PointCursor
consPointCursor i (PointCursor ph j) = PointCursor (consPath i ph) j

unconsPointCursor :: PointCursor -> (KidIndex /\ PointCursor) \/ PointIndex
unconsPointCursor (PointCursor ph j) = case unconsPath ph of
  Just (i /\ ph') -> Left (i /\ PointCursor ph' j)
  Nothing -> Right j

atPointCursor :: forall n a. (Sexp n a -> Sexp n a) -> PointCursor -> Sexp n a -> (Sexp n a -> Sexp n a)
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
-- SpanCursor
--------------------------------------------------------------------------------

data SpanCursor = SpanCursor Path PointCursor PointCursor

-- | safe constructor. checks:
-- |   - p1 <= p2
spanCursor :: Path -> PointCursor -> PointCursor -> SpanCursor
spanCursor = SpanCursor -- TODO

--------------------------------------------------------------------------------
-- ZipperCursor
--------------------------------------------------------------------------------

data ZipperCursor = ZipperCursor SpanCursor SpanCursor

-- | safe constructor. checks:
-- |   - s2 is non-empty
zipperCursor :: SpanCursor -> SpanCursor -> ZipperCursor
zipperCursor = ZipperCursor -- TODO

--------------------------------------------------------------------------------
-- Cursor
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- misc
--------------------------------------------------------------------------------

inSexp' :: forall n a. Sexp' n a -> (Sexp n a -> Sexp' n a) /\ Sexp n a
inSexp' (Atom _) = bug "inSexp'" "can't go in an Atom"
inSexp' (Group e) = Group /\ e