module Sexpze.Data.Sexp where

import Prelude

import Data.Array as Array
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Sexp a
  = Group (Array (Sexp a))
  | Atom a

derive instance Generic (Sexp a) _

instance Show a => Show (Sexp a) where
  show x = genericShow x

instance Eq a => Eq (Sexp a) where
  eq x y = genericEq x y

toString :: forall a. Show a => Sexp a -> String
toString (Group xs) = "(" <> (xs # map show # Array.intercalate " ") <> ")"
toString (Atom a) = show a

