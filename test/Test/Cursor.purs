module Test.Cursor where

import Prelude

import Data.List (List(..), (:))
import Data.List as List
import Data.Newtype (wrap)
import Sexpze.Data.Sexp (Sexp, Sexp'(..))
import Sexpze.Data.Sexp.Cursor (Cursor(..), Point(..), SpanCursor(..), ZipperCursor(..), dragFromPoint)
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
  Spec.describe "most basic" do
    let
      xs :: Sexp String
      xs = [ Group [] ] :: Sexp String
    it "don't move" do
      shouldEqual
        (dragFromPoint (point [] 0) (point [] 0) xs)
        (InjectPoint $ point [] 0)
    it "1-wide span" do
      shouldEqual
        (dragFromPoint (point [] 0) (point [] 1) xs)
        (InjectSpanCursor $ spanCursor [] 0 1)
    it "1-deep zipper" do
      shouldEqual
        (dragFromPoint (point [] 0) (Point (wrap 0 : Nil) (wrap 0)) xs)
        -- (InjectZipperCursor (ZipperCursor (spanCursor [] 0 1) (spanCursor [ 0 ] 0 0)))
        (InjectZipperCursor $ zipperCursor [] 0 1 [ 0 ] 0 0)
      pure unit
  Spec.describe "shallow" do
    let
      xs :: Sexp String
      xs = [ Atom "a", Atom "b", Atom "c" ]
    it "don't move" do
      shouldEqual
        (dragFromPoint (point [] 0) (point [] 0) xs)
        (InjectPoint $ point [] 0)
      pure unit
    it "1-wide span" do
      shouldEqual
        (dragFromPoint (point [] 0) (point [] 1) xs)
        (InjectSpanCursor $ spanCursor [] 0 1)
    it "2-wide span" do
      shouldEqual
        (dragFromPoint (point [] 1) (Point Nil (wrap 3)) xs)
        (InjectSpanCursor $ spanCursor [] 1 3)
      pure unit
  Spec.describe "deeper" do
    let xs = [ Atom "a", Group [ Atom "b", Atom "c" ], Atom "d" ] :: Sexp String
    it "deeper span" do
      shouldEqual
        (dragFromPoint (point [ 1 ] 0) (point [ 1 ] 1) xs)
        (InjectSpanCursor $ spanCursor [ 1 ] 0 1)
    it "simplest zipper" do
      shouldEqual
        (dragFromPoint (point [] 1) (point [ 1 ] 0) xs)
        (InjectZipperCursor $ zipperCursor [] 0 1 [ 1 ] 0 2)
    pure unit
  pure unit

--------------------------------------------------------------------------------
-- utilities
--------------------------------------------------------------------------------

point :: Array Int -> Int -> Point
point is j = Point (is # map wrap # List.fromFoldable) (wrap j)

spanCursor :: Array Int -> Int -> Int -> SpanCursor
spanCursor is j1 j2 = SpanCursor (is # map wrap # List.fromFoldable) (wrap j1) (wrap j2)

zipperCursor :: Array Int -> Int -> Int -> Array Int -> Int -> Int -> ZipperCursor
zipperCursor is1 j1 j1' is2 j2 j2' = ZipperCursor (spanCursor is1 j1 j1') (spanCursor is2 j2 j2')