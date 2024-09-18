module Test.Cursor.Drag where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect.Exception (Error)
import Sexpze.Data.Sexp (Sexp(..), Sexp'(..))
import Sexpze.Data.Sexp.Cursor (Cursor(..), KidIndex(..), Path(..), PointCursor(..), Span(..), SpanCursor(..), SpanHandle(..), ZipperCursor(..), ZipperHandle(..), emptyZipperCursor)
import Sexpze.Data.Sexp.Cursor.Drag (dragFromCursor)
import Sexpze.Utility (todo)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

shouldEqual :: forall m5 a13. Applicative m5 => Eq a13 => MonadThrow Error m5 => Show a13 => a13 -> a13 -> m5 Unit
shouldEqual a b = unless (a == b) $ fail $ show a <> " ≠\n  " <> show b

spec :: Spec Unit
spec = do
  describe "basic" do
    let e = fromTreeToSpan [ L "a", L "b", L "c" ]
    describe "point" do
      it "point" do
        shouldEqual
          ( dragFromCursor
              (mkCursor [] 0 3 [] 0 0 (Inner Start))
              (mkPointCursor [] 0)
              e
          )
          (mkCursor [] 0 3 [] 0 0 (Inner Start))
    describe "span" do
      it "to end" do
        shouldEqual
          ( dragFromCursor
              (mkCursor [] 0 3 [] 0 0 (Inner Start))
              (mkPointCursor [] 1)
              e
          )
          (mkCursor [] 0 2 [] 0 0 (Inner Start))
      it "to start" do
        shouldEqual
          ( dragFromCursor
              (mkCursor [] 1 2 [] 0 0 (Inner Start))
              (mkPointCursor [] 0)
              e
          )
          (mkCursor [] 0 2 [] 0 0 (Inner Start))
  describe "deeper" do
    let e = fromTreeToSpan [ L "a", B [ L "b", L "c", L "d" ], L "e" ]
    describe "point" do
      it "point" do
        shouldEqual
          ( dragFromCursor
              (mkCursor [ 1 ] 0 3 [] 0 0 (Inner Start))
              (mkPointCursor [ 1 ] 0)
              e
          )
          (mkCursor [ 1 ] 0 3 [] 0 0 (Inner Start))
    describe "span" do
      it "start to end" do
        shouldEqual
          ( dragFromCursor
              (mkCursor [ 1 ] 0 3 [] 0 0 (Inner Start))
              (mkPointCursor [ 1 ] 1)
              e
          )
          (mkCursor [ 1 ] 0 2 [] 0 0 (Inner Start))
      it "end to start" do
        shouldEqual
          ( dragFromCursor
              (mkCursor [ 1 ] 1 2 [] 0 0 (Inner Start))
              (mkPointCursor [ 1 ] 0)
              e
          )
          (mkCursor [ 1 ] 0 2 [] 0 0 (Inner Start))
  describe "basic zippers" do
    let e = fromTreeToSpan [ L "a", B [ L "b", L "c", L "d" ], L "e" ]
    it "outer start to inner start" do
      shouldEqual
        ( dragFromCursor
            (mkCursor [] 1 2 [] 0 0 (Inner Start))
            (mkPointCursor [ 1 ] 0)
            e
        )
        (mkCursor [] 1 1 [ 1 ] 0 0 (Inner Start))
    it "inner start to outer start" do
      shouldEqual
        ( dragFromCursor
            (mkCursor [ 1 ] 0 3 [] 0 0 (Inner Start))
            (mkPointCursor [] 1)
            e
        )
        (mkCursor [] 1 1 [ 1 ] 0 3 (Outer Start))
    -- it "inner end to outer end" do
    --   shouldEqual
    --     ( dragFromCursor
    --         (mkCursor [ 1 ] 3 0 [] 0 0 (Inner Start))
    --         (mkPointCursor [] 2)
    --         e
    --     )
    --     (mkCursor [] 1 1 [ 1 ] 0 3 (Outer Start))
    -- it "outer end to inner end" do
    --   pure unit
    pure unit

--------------------------------------------------------------------------------

data Tree = B (Array Tree) | L String

fromTreeToSpan :: Array Tree -> Span {} String
fromTreeToSpan = Span <<< map toSexp'

toSexp' :: Tree -> Sexp' {} String -- Span {} String
toSexp' (B ts) = Group (Sexp {} (ts # map toSexp'))
toSexp' (L s) = Atom s

span :: Array (Sexp' {} String) -> Span {} String
span es = Span es

mkCursor :: Array Int -> Int -> Int -> Array Int -> Int -> Int -> ZipperHandle -> Cursor
mkCursor ph_outer d1_outer d2_outer ph_inner d1_inner d2_inner h =
  Cursor
    ( ZipperCursor
        (SpanCursor (mkPath ph_outer) (wrap d1_outer) (wrap d2_outer))
        (SpanCursor (mkPath ph_inner) (wrap d1_inner) (wrap d2_inner))
    )
    h

mkPath :: Array Int -> Path
mkPath is = is # map (wrap :: _ -> KidIndex) # List.fromFoldable # Path

mkPointCursor :: Array Int -> Int -> PointCursor
mkPointCursor is j = PointCursor (mkPath is) (wrap j)

