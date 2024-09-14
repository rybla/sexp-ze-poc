module Sexpze.Data.Sexp where

import Prelude

import Data.Array as Array
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

--------------------------------------------------------------------------------
-- Sexp
--------------------------------------------------------------------------------

-- | Type parameters:
-- |   - `n` is data at each `Sexp`
-- |   - `a` is data at each `Atom`
data Sexp node atom = Sexp node (Array (Sexp' node atom))
data Sexp' node atom = Atom atom | Group (Sexp node atom)

derive instance Generic (Sexp node atom) _
derive instance Generic (Sexp' node atom) _

instance (Show node, Show atom) => Show (Sexp node atom) where
  show x = genericShow x

instance (Show node, Show atom) => Show (Sexp' node atom) where
  show x = genericShow x

instance (Eq node, Eq atom) => Eq (Sexp node atom) where
  eq x = genericEq x

instance (Eq node, Eq atom) => Eq (Sexp' node atom) where
  eq x = genericEq x

prettySexp :: forall node atom. Show node => Show atom => Sexp node atom -> String
prettySexp (Sexp node sexp') = show node <> " " <> (sexp' # map prettySexp' # Array.intercalate " ")

prettySexp' :: forall node atom. Show node => Show atom => Sexp' node atom -> String
prettySexp' (Atom atom) = show atom
prettySexp' (Group sexp) = prettySexp sexp

