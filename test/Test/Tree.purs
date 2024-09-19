module Test.Tree where

import Prelude

import Test.Common (Term, Term', TermSpan)
import Sexpze.Data.Sexp (Sexp(..), Sexp'(..))
import Sexpze.Data.Sexp.Cursor (Span(..))

data Tree = B (Array Tree) | L String

fromTreeToTerm :: Array Tree -> Term
fromTreeToTerm = Sexp {} <<< map fromTreeToTerm'

fromTreeToTermSpan :: Array Tree -> TermSpan
fromTreeToTermSpan = Span <<< map fromTreeToTerm'

fromTreeToTerm' :: Tree -> Term'
fromTreeToTerm' (B ts) = Group (Sexp {} (ts # map fromTreeToTerm'))
fromTreeToTerm' (L s) = Atom s