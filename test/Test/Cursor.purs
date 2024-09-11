module Test.Cursor where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.List (List(..), (:))
import Data.List as List
import Data.Newtype (wrap)
import Effect.Exception (Error)
import Effect.Exception as Exception
import Sexpze.Data.Sexp (Sexp, Sexp'(..))
import Sexpze.Data.Sexp.Cursor (Cursor(..), Point(..), SpanCursor(..), ZipperCursor(..), dragFromPoint, prettySexpWithCursor)
import Sexpze.Utility (todo)
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions as Assertions

spec :: Spec Unit
spec = Spec.describe "Cursor" do
  spec_dragFromPoint
  pure unit

shouldEqualCursor :: forall m a. MonadThrow Error m => Show a => Sexp a -> Cursor -> Cursor -> m Unit
shouldEqualCursor xs c1 c2 = unless (c1 == c2) do
  -- Assertions.fail (prettySexpWithCursor c1 xs <> " ≠ " <> prettySexpWithCursor c2 xs)
  Assertions.fail (show c1 <> " ≠ " <> show c2)

spec_dragFromPoint :: Spec Unit
spec_dragFromPoint = Spec.describe "dragFromPoint" do
  Spec.describe "most basic" do
    let xs = [ Group [] ] :: Sexp String
    Spec.it "don't move" do
      shouldEqualCursor xs
        (dragFromPoint (point [] 0) (point [] 0) xs)
        (InjectPoint $ point [] 0)
    Spec.it "1-wide span" do
      shouldEqualCursor xs
        (dragFromPoint (point [] 0) (point [] 1) xs)
        (InjectSpanCursor $ spanCursor [] 0 1)
    Spec.it "1-deep zipper" do
      shouldEqualCursor xs
        (dragFromPoint (point [] 0) (Point (wrap 0 : Nil) (wrap 0)) xs)
        (InjectZipperCursor $ zipperCursor [] 0 1 [ 0 ] 0 0)
      pure unit
  Spec.describe "shallow" do
    let xs = [ atom "a", atom "b", atom "c" ]
    Spec.it "don't move" do
      shouldEqualCursor xs
        (dragFromPoint (point [] 0) (point [] 0) xs)
        (InjectPoint $ point [] 0)
      pure unit
    Spec.it "1-wide span" do
      shouldEqualCursor xs
        (dragFromPoint (point [] 0) (point [] 1) xs)
        (InjectSpanCursor $ spanCursor [] 0 1)
    Spec.it "2-wide span" do
      shouldEqualCursor xs
        (dragFromPoint (point [] 1) (Point Nil (wrap 3)) xs)
        (InjectSpanCursor $ spanCursor [] 1 3)
      pure unit
  Spec.describe "deeper" do
    let xs = [ atom "a", Group [ atom "b", atom "c" ], atom "d" ]
    Spec.it "point" do
      shouldEqualCursor xs
        (dragFromPoint (point [ 1 ] 0) (point [ 1 ] 0) xs)
        (InjectPoint $ point [ 1 ] 0)
    Spec.it "span from left to right" do
      shouldEqualCursor xs
        (dragFromPoint (point [ 1 ] 0) (point [ 1 ] 1) xs)
        (InjectSpanCursor $ spanCursor [ 1 ] 0 1)
    Spec.it "span from right to left" do
      shouldEqualCursor xs
        (dragFromPoint (point [ 1 ] 1) (point [ 1 ] 0) xs)
        (InjectSpanCursor $ spanCursor [ 1 ] 0 1)
    Spec.it "zipper from outer left to inner left" do
      shouldEqualCursor xs
        (dragFromPoint (point [] 1) (point [ 1 ] 0) xs)
        (InjectZipperCursor $ zipperCursor [] 1 2 [ 1 ] 0 2)
    Spec.it "zipper from inner left to outer left" do
      shouldEqualCursor xs
        (dragFromPoint (point [ 1 ] 0) (point [] 1) xs)
        (InjectZipperCursor $ zipperCursor [] 1 2 [ 1 ] 0 2)
    pure unit
  Spec.describe "even deeper" do
    let xs = [ atom "a", Group [ atom "b", Group [ atom "c", atom "d", atom "e" ], atom "f" ], atom "g" ]
    Spec.it "span from left to right" do
      shouldEqualCursor xs
        (dragFromPoint (point [ 1, 1 ] 1) (point [ 1, 1 ] 2) xs)
        (InjectSpanCursor $ spanCursor [ 1, 1 ] 1 2)
    Spec.it "span from right to left" do
      shouldEqualCursor xs
        (dragFromPoint (point [ 1, 1 ] 2) (point [ 1, 1 ] 1) xs)
        (InjectSpanCursor $ spanCursor [ 1, 1 ] 1 2)
    Spec.it "zipper from outer left to inner left" do
      shouldEqualCursor xs
        (dragFromPoint (point [ 1 ] 1) (point [ 1, 1 ] 1) xs)
        (InjectZipperCursor $ zipperCursor [ 1 ] 1 2 [ 1 ] 1 3)
    Spec.it "zipper from inner left to outer left" do
      shouldEqualCursor xs
        (dragFromPoint (point [ 1, 1 ] 1) (point [ 1 ] 1) xs)
        (InjectZipperCursor $ zipperCursor [ 1 ] 1 2 [ 1 ] 1 3)
    pure unit
  pure unit

--------------------------------------------------------------------------------
-- utilities
--------------------------------------------------------------------------------

newtype UnquotedString = UnquotedString String

derive newtype instance Eq UnquotedString
derive newtype instance Ord UnquotedString

instance Show UnquotedString where
  show (UnquotedString s) = s

atom :: String -> Sexp' UnquotedString
atom = Atom <<< UnquotedString

point :: Array Int -> Int -> Point
point is j = Point (is # map wrap # List.fromFoldable) (wrap j)

spanCursor :: Array Int -> Int -> Int -> SpanCursor
spanCursor is j1 j2 = SpanCursor (is # map wrap # List.fromFoldable) (wrap j1) (wrap j2)

zipperCursor :: Array Int -> Int -> Int -> Array Int -> Int -> Int -> ZipperCursor
zipperCursor is1 j1 j1' is2 j2 j2' = ZipperCursor (spanCursor is1 j1 j1') (spanCursor is2 j2 j2')