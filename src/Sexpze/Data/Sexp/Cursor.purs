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
import Sexpze.Data.Sexp (Sexp(..), Sexp', SexpItem)
import Sexpze.Utility (bug, todo)

-- a cursor into a Sexp with an outer Path, an middle (selected) Path, and an
-- inner Sexp
data Cursor p a = Cursor (Path p a) (Path p a) (Sexp p a)

-- a path into a Sexp to a sub-Sexp
data Path p a = Path (List (NarrowTooth p a)) (WideTooth p a)

-- a tooth around a sub-Sexp 
data WideTooth p a = WideTooth (Array (SexpItem p a)) (Array (SexpItem p a))

-- a tooth around a Group
data NarrowTooth p a = NarrowTooth (Array (SexpItem p a)) (Array (SexpItem p a))

