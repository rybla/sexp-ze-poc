module Sexpze.Data.Sexp.Cursor where

import Prelude

import Control.Plus (empty)
import Data.Array as Array
import Data.List (List(..), (:))
import Data.Maybe (Maybe, fromMaybe')
import Data.Newtype (class Newtype, over2)
import Data.Newtype as Newtype
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Sexpze.Data.Sexp (Sexp(..), Sexp')
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

--------------------------------------------------------------------------------
-- Path
--------------------------------------------------------------------------------

newtype Path = Path (List KidIndex)

unconsPath :: Path -> Maybe (KidIndex /\ Path)
unconsPath (Path Nil) = empty
unconsPath (Path (i : is)) = pure (i /\ Path is)

consPath :: KidIndex -> Path -> Path
consPath i (Path is) = Path (i : is)

instance Semigroup Path where
  append (Path ph) (Path Nil) = Path ph
  append (Path Nil) (Path ph) = Path ph
  append (Path (i1 : Nil)) (Path (i2 : ph2)) = Path $ over2 KidIndex add i1 i2 : ph2
  append (Path (i1 : ph1)) (Path ph2) = consPath i1 (Path ph1 <> Path ph2)

--------------------------------------------------------------------------------
-- PointCursor
--------------------------------------------------------------------------------

data PointCursor = PointCursor Path PointIndex

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

