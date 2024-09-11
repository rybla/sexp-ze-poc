module Test.Cursor where

import Prelude

import Data.List (List(..))
import Data.Newtype (wrap)
import Sexpze.Data.Sexp (Sexp, Sexp'(..))
import Sexpze.Data.Sexp.Cursor (Point(..), SpanCursor(..), dragFromPoint)
import Sexpze.Data.Sexp.Cursor as Cursor
import Sexpze.Utility (todo)
import Test.Spec (Spec, it)
import Test.Spec as Spec
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = Spec.describe "Cursor" do
  spec_dragFromPoint
  pure unit

spec_dragFromPoint :: Spec Unit
spec_dragFromPoint = Spec.describe "dragFromPoint" do
  Spec.describe "shallow" do
    let xs = [ Atom "a", Atom "b", Atom "c" ] :: Sexp String
    it "don't move" do
      shouldEqual
        (dragFromPoint (Point Nil (wrap 0)) (Point Nil (wrap 0)) xs)
        (Cursor.InjectPoint $ Point Nil (wrap 0))
      pure unit
    it "1-wide span" do
      shouldEqual
        (dragFromPoint (Point Nil (wrap 0)) (Point Nil (wrap 1)) xs)
        (Cursor.InjectSpanCursor $ SpanCursor Nil (wrap 0) (wrap 1))
    it "2-wide span" do
      shouldEqual
        (dragFromPoint (Point Nil (wrap 1)) (Point Nil (wrap 3)) xs)
        (Cursor.InjectSpanCursor $ SpanCursor Nil (wrap 1) (wrap 3))
      pure unit
  pure unit

