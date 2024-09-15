module Sexpze.Data.Sexp.Cursor where

import Prelude

import Control.Plus (empty)
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/), Either5, in1, in2, in3, in4, in5)
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

modifySpanAt :: forall n a. KidIndex -> (Sexp' n a -> Sexp' n a) -> Span n a -> Span n a
modifySpanAt (KidIndex i) f (Span e's) = Span (e's # Array.modifyAt i f # fromMaybe' (\_ -> bug "modifySpanAt" "KidIndex out of bounds"))

getSexpKid :: forall n a. KidIndex -> Sexp n a -> Sexp' n a
getSexpKid (KidIndex i) (Sexp _ e's) = e's Array.!! i # fromMaybe' (\_ -> bug "getSexpKid" "KidIndex out of bounds")

getSpanKid :: forall n a. KidIndex -> Span n a -> Sexp' n a
getSpanKid (KidIndex i) (Span e's) = e's Array.!! i # fromMaybe' (\_ -> bug "getSexpKid" "KidIndex out of bounds")

atSexpKidIndex :: forall n a. KidIndex -> Sexp n a -> (Sexp' n a -> Sexp n a) /\ Sexp' n a
atSexpKidIndex i e =
  Tuple
    (\e' -> e # modifySexpAt i (const e'))
    (e # getSexpKid i)

atSpanKidIndex :: forall n a. KidIndex -> Span n a -> (Sexp' n a -> Span n a) /\ Sexp' n a
atSpanKidIndex i e =
  Tuple
    (\e' -> e # modifySpanAt i (const e'))
    (e # getSpanKid i)

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
  Just (i /\ ph') -> case e # atSexpKidIndex i of
    _ /\ Atom _ -> bug "atPath" "Path into Atom"
    wrap_i /\ Group e_i -> lmap ((wrap_i <<< Group) <<< _) $ atPath ph' e_i

atSpanPath :: forall n a. Path -> Span n a -> (Span n a -> Span n a) /\ Span n a
atSpanPath ph e = case unconsPath ph of
  Nothing -> identity /\ e
  Just (i /\ ph') -> case e # atSpanKidIndex i of
    _ /\ Atom _ -> bug "atSpanPath" "Path into Atom"
    -- wrap_i /\ Group e_i -> lmap ((wrap_i <<< Group <<< ?a) <<< _) $ atSpanPath ph' (e_i # toSpan)
    wrap_i /\ Group e_i -> todo "" {}

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
      w' /\ e' = e # atSexpKidIndex i
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

data SpanCursor = SpanCursor Path PointIndex PointIndex

derive instance Generic SpanCursor _

instance Show SpanCursor where
  show x = genericShow x

instance Eq SpanCursor where
  eq x = genericEq x

data Span n a = Span (Array (Sexp' n a))

derive instance Generic (Span n a) _

instance (Show n, Show a) => Show (Span n a) where
  show x = genericShow x

instance (Eq n, Eq a) => Eq (Span n a) where
  eq x = genericEq x

toSpan :: forall n a. Sexp n a -> Span n a
toSpan (Sexp _ es) = Span es

-- | safe constructor. checks:
-- |   - p1 <= p2
spanCursor :: Path -> PointIndex -> PointIndex -> SpanCursor
spanCursor = SpanCursor -- TODO

unconsSpanCursor
  :: SpanCursor
  -> Either
       (KidIndex /\ SpanCursor)
       (PointIndex /\ PointIndex)
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

atSpanPointIndexSpan :: forall n a. PointIndex -> PointIndex -> Span n a -> (Span n a -> Span n a) /\ Span n a
atSpanPointIndexSpan i1 i2 (Span es) =
  let
    { before, after: after_ } = es # Array.splitAt (unwrap i1)
    { before: middle, after } = after_ # Array.splitAt (unwrap i2 - unwrap i1)
  in
    Tuple
      (\(Span es') -> Span (before <> es' <> after))
      (Span middle)

atSpanCursor :: forall n a. SpanCursor -> Sexp n a -> (Span n a -> Sexp n a) /\ Span n a
atSpanCursor (SpanCursor ph j1 j2) =
  atPath ph >>> \(w /\ e') ->
    lmap (w <<< _)
      $ atPointIndexSpan j1 j2 e'

atSpanCursor' :: forall n a. SpanCursor -> Span n a -> (Span n a -> Span n a) /\ Span n a
atSpanCursor' (SpanCursor ph j1 j2) =
  atSpanPath ph >>> \(w /\ e') ->
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

data Zipper n a = Zipper PointCursor (Span n a)

derive instance Generic (Zipper n a) _

instance (Show n, Show a) => Show (Zipper n a) where
  show x = genericShow x

instance (Eq n, Eq a) => Eq (Zipper n a) where
  eq x = genericEq x

unconsZipperCursor
  :: ZipperCursor \/ SpanCursor
  -> Either5
       -- midst of outer path
       (KidIndex /\ ZipperCursor)
       -- end of outer path; midst of inner path
       ((PointIndex /\ PointIndex) /\ KidIndex /\ SpanCursor)
       -- end of outer path; end of inner path since it's empty
       ((PointIndex /\ PointIndex) /\ (PointIndex /\ PointIndex))
       -- midst of inner path
       (KidIndex /\ SpanCursor)
       -- end of inner path
       (PointIndex /\ PointIndex)
unconsZipperCursor (Left (ZipperCursor s1 s2)) | Left (i1 /\ s1') <- unconsSpanCursor s1 = in1 (i1 /\ ZipperCursor s1' s2)
unconsZipperCursor (Left (ZipperCursor s1 s2)) | Right (j1_start /\ j1_end) <- unconsSpanCursor s1, Left (i2 /\ s2') <- unconsSpanCursor s2 = in2 ((j1_start /\ j1_end) /\ i2 /\ s2')
unconsZipperCursor (Left (ZipperCursor s1 s2)) | Right (j1_start /\ j1_end) <- unconsSpanCursor s1, Right (j2_start /\ j2_end) <- unconsSpanCursor s2 = in3 ((j1_start /\ j1_end) /\ (j2_start /\ j2_end))
unconsZipperCursor (Right s) | Left (i /\ s') <- unconsSpanCursor s = in4 (i /\ s')
unconsZipperCursor (Right s) | Right (j1 /\ j2) <- unconsSpanCursor s = in5 (j1 /\ j2)
unconsZipperCursor _ = bug "unconsZipperCursor" "impossible"

atZipperCursor :: forall n a. ZipperCursor -> Sexp n a -> ((Span n a -> Span n a) -> Sexp n a) /\ Zipper n a
atZipperCursor (ZipperCursor s1 s2) e =
  let
    w1 /\ e1 = e # atSpanCursor s1
    w2 /\ e2 = e1 # atSpanCursor' s2
  in
    -- Tuple
    --   ?a
    --   (Zipper ?a ?A)
    todo "" {}

--------------------------------------------------------------------------------
-- Cursor
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- misc
--------------------------------------------------------------------------------

inSexp' :: forall n a. Sexp' n a -> (Sexp n a -> Sexp' n a) /\ Sexp n a
inSexp' (Atom _) = bug "inSexp'" "can't go in an Atom"
inSexp' (Group e) = Group /\ e