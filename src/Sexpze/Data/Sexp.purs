module Sexpze.Data.Sexp where

import Prelude

import Data.Array as Array
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))

--------------------------------------------------------------------------------
-- Sexp
--------------------------------------------------------------------------------

-- | Type parameters:
-- |   - `point` is data at point in a `Sexp`
-- |   - `atom` is the data at each `Atom` in the `Sexp`
newtype Sexp point atom = Sexp (Array (SexpItem point atom))
type SexpItem point atom = point /\ Sexp' point atom
data Sexp' point atom = Atom atom | Group (Sexp point atom)

derive instance Generic (Sexp point atom) _
derive instance Generic (Sexp' point atom) _

instance (Show point, Show atom) => Show (Sexp point atom) where
  show x = genericShow x

instance (Show point, Show atom) => Show (Sexp' point atom) where
  show x = genericShow x

instance (Eq point, Eq atom) => Eq (Sexp point atom) where
  eq x = genericEq x

instance (Eq point, Eq atom) => Eq (Sexp' point atom) where
  eq x = genericEq x

prettySexp :: forall point atom. Show point => Show atom => Sexp point atom -> String
prettySexp (Sexp xs) = xs # map (\(_ /\ x) -> prettySexp' x) # Array.intercalate " "

prettySexp' :: forall point atom. Show point => Show atom => Sexp' point atom -> String
prettySexp' (Atom atom) = show atom
prettySexp' (Group sexp) = prettySexp sexp

