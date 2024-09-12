module Sexpze.Data.Sexp where

import Prelude

import Data.Array as Array
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

type Sexp n a = Array (Sexp' n a)

data Sexp' n a
  = Atom a
  | Group n (Sexp n a)

derive instance Generic (Sexp' n a) _

instance (Show n, Show a) => Show (Sexp' n a) where
  show x = genericShow x

instance (Eq n, Eq a) => Eq (Sexp' n a) where
  eq x y = genericEq x y

toString :: forall n a. Show a => Sexp' n a -> String
toString (Group _ xs) = "(" <> (xs # map toString # Array.intercalate " ") <> ")"
toString (Atom a) = show a
