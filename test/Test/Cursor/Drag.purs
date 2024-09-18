module Test.Cursor.Drag where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array as Array
import Data.List as List
import Data.Newtype (wrap)
import Effect.Exception (Error)
import Sexpze.Data.Sexp (Sexp(..), Sexp'(..))
import Sexpze.Data.Sexp.Cursor (Cursor(..), KidIndex, Path(..), PointCursor(..), Span(..), SpanCursor(..), SpanHandle(..), ZipperCursor(..), ZipperHandle(..), fromPointCursorToCursor)
import Sexpze.Data.Sexp.Cursor.Drag (dragFromCursor)
import Sexpze.Data.Sexp.Cursor.Pretty (indent, prettyTermWithCursor)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (fail)

--------------------------------------------------------------------------------

type Term = Sexp {} String
type Term' = Sexp' {} String
type TermSpan = Span {} String

--------------------------------------------------------------------------------

spec :: Spec Unit
spec = do
  describe "basic" do
    let e = fromTreeToTermSpan [ L "a", L "b", L "c" ]
    describe "point" do
      it "point" do
        should_dragFromCursor e
          (mkCursor [] 0 3 [] 0 0 (Inner Start))
          (mkPointCursor [] 0)
          (mkCursor [] 0 3 [] 0 0 (Inner Start))
    describe "span" do
      it "to end" do
        shouldEqualCursor e
          ( dragFromCursor
              (mkCursor [] 0 3 [] 0 0 (Inner Start))
              (mkPointCursor [] 1)
              e
          )
          (mkCursor [] 0 2 [] 0 0 (Inner Start))
      it "to start" do
        shouldEqualCursor e
          ( dragFromCursor
              (mkCursor [] 1 2 [] 0 0 (Inner Start))
              (mkPointCursor [] 0)
              e
          )
          (mkCursor [] 0 2 [] 0 0 (Inner Start))
  describe "deeper" do
    let e = fromTreeToTermSpan [ L "a", B [ L "b", L "c", L "d" ], L "e" ]
    describe "point" do
      it "point" do
        shouldEqualCursor e
          ( dragFromCursor
              (mkCursor [ 1 ] 0 3 [] 0 0 (Inner Start))
              (mkPointCursor [ 1 ] 0)
              e
          )
          (mkCursor [ 1 ] 0 3 [] 0 0 (Inner Start))
    describe "span" do
      it "start to end" do
        shouldEqualCursor e
          ( dragFromCursor
              (mkCursor [ 1 ] 0 3 [] 0 0 (Inner Start))
              (mkPointCursor [ 1 ] 1)
              e
          )
          (mkCursor [ 1 ] 0 2 [] 0 0 (Inner Start))
      it "end to start" do
        shouldEqualCursor e
          ( dragFromCursor
              (mkCursor [ 1 ] 1 2 [] 0 0 (Inner Start))
              (mkPointCursor [ 1 ] 0)
              e
          )
          (mkCursor [ 1 ] 0 2 [] 0 0 (Inner Start))
  describe "basic zippers" do
    let e = fromTreeToTermSpan [ L "a", B [ L "b", L "c", L "d" ], L "e" ]
    it "outer start to inner start" do
      shouldEqualCursor e
        ( dragFromCursor
            (mkCursor [] 1 2 [] 0 0 (Inner Start))
            (mkPointCursor [ 1 ] 0)
            e
        )
        (mkCursor [] 1 1 [ 1 ] 0 0 (Inner Start))
    it "inner start to outer start" do
      shouldEqualCursor e
        ( dragFromCursor
            (mkCursor [ 1 ] 0 3 [] 0 0 (Inner Start))
            (mkPointCursor [] 1)
            e
        )
        (mkCursor [] 1 1 [ 1 ] 0 3 (Outer Start))
    it "inner end to outer end" do
      should_dragFromCursor e
        (mkCursor [ 1 ] 3 0 [] 0 0 (Inner Start))
        (mkPointCursor [] 2)
        (mkCursor [] 1 1 [ 1 ] 3 0 (Outer End))
    pending "outer end to inner end"
    pure unit

--------------------------------------------------------------------------------

shouldEqual :: forall m a. Applicative m => Eq a => MonadThrow Error m => Show a => a -> a -> m Unit
shouldEqual a b = unless (a == b) $ fail $ show a <> " ≠\n  " <> show b <> "\n"

shouldEqualCursor :: forall m. Applicative m => MonadThrow Error m => TermSpan -> Cursor -> Cursor -> m Unit
shouldEqualCursor (Span es) c1 c2 = unless (c1 == c2) $ fail $ indent 2 $ "\n" <> prettyTermWithCursor c1 (Sexp {} es) <> "\n≠\n" <> prettyTermWithCursor c2 (Sexp {} es) <> "\n"

should_dragFromCursor :: forall m. MonadThrow Error m => TermSpan -> Cursor -> PointCursor -> Cursor -> m Unit
should_dragFromCursor e@(Span es) c1 p2 c =
  let
    c' = dragFromCursor c1 p2 e
  in
    unless (c' == c) do
      fail $ indent 2 $ "\n" <> Array.foldMap (_ <> "\n")
        [ "drag from\n"
        , prettyTermWithCursor c1 (Sexp {} es)
        , "\nto\n"
        , prettyTermWithCursor (fromPointCursorToCursor p2 e) (Sexp {} es)
        , "\nis supposed to be\n"
        , prettyTermWithCursor c (Sexp {} es)
        , "\nbut actually is\n"
        , prettyTermWithCursor c' (Sexp {} es)
        ]

--------------------------------------------------------------------------------

data Tree = B (Array Tree) | L String

fromTreeToTerm :: Array Tree -> Term
fromTreeToTerm = Sexp {} <<< map toSexp'

fromTreeToTermSpan :: Array Tree -> TermSpan
fromTreeToTermSpan = Span <<< map toSexp'

toSexp' :: Tree -> Term'
toSexp' (B ts) = Group (Sexp {} (ts # map toSexp'))
toSexp' (L s) = Atom s

span :: Array (Sexp' {} String) -> TermSpan
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

