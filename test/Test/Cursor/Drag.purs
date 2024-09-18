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
import Sexpze.Data.Sexp.Cursor.Pretty (indent, prettyPointCursor, prettyTermWithCursor)
import Sexpze.Utility (todo)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (fail)

--------------------------------------------------------------------------------

type Term = Sexp {} String
type Term' = Sexp' {} String
type TermSpan = Span {} String

--------------------------------------------------------------------------------

spec :: Spec Unit
spec = do
  describe "dragFromPointCursor" do
    describe "basic" do
      let e = fromTreeToTermSpan [ L "a", L "b", L "c" ]
      describe "point" do
        e # it_should_dragFromPointCursor "" (mkPointCursor [] 0) (mkPointCursor [] 0) (mkCursor [] 0 3 [] 0 0 (Inner Start))
        e # it_should_dragFromPointCursor "" (mkPointCursor [] 1) (mkPointCursor [] 1) (mkCursor [] 1 2 [] 0 0 (Inner Start))
        e # it_should_dragFromPointCursor "" (mkPointCursor [] 2) (mkPointCursor [] 2) (mkCursor [] 2 1 [] 0 0 (Inner Start))
        e # it_should_dragFromPointCursor "" (mkPointCursor [] 3) (mkPointCursor [] 3) (mkCursor [] 3 0 [] 0 0 (Inner Start))
      describe "span" do
        e # it_should_dragFromPointCursor "to end " (mkPointCursor [] 0) (mkPointCursor [] 1) (mkCursor [] 0 2 [] 0 0 (Inner Start))
        e # it_should_dragFromPointCursor "to start " (mkPointCursor [] 1) (mkPointCursor [] 0) (mkCursor [] 0 2 [] 0 0 (Inner Start))
    describe "deeper" do
      let e = fromTreeToTermSpan [ L "a", B [ L "b", L "c", L "d" ], L "e" ]
      describe "point" do
        e # it_should_dragFromPointCursor "" (mkPointCursor [ 1 ] 0) (mkPointCursor [ 1 ] 0) (mkCursor [ 1 ] 0 3 [] 0 0 (Inner Start))
        e # it_should_dragFromPointCursor "" (mkPointCursor [ 1 ] 1) (mkPointCursor [ 1 ] 1) (mkCursor [ 1 ] 1 2 [] 0 0 (Inner Start))
        e # it_should_dragFromPointCursor "" (mkPointCursor [ 1 ] 2) (mkPointCursor [ 1 ] 2) (mkCursor [ 1 ] 2 1 [] 0 0 (Inner Start))
        e # it_should_dragFromPointCursor "" (mkPointCursor [ 1 ] 3) (mkPointCursor [ 1 ] 3) (mkCursor [ 1 ] 3 0 [] 0 0 (Inner Start))
      describe "span" do
        e # it_should_dragFromPointCursor "start to end " (mkPointCursor [ 1 ] 0) (mkPointCursor [ 1 ] 1) (mkCursor [ 1 ] 0 2 [] 0 0 (Inner Start))
        e # it_should_dragFromPointCursor "end to start " (mkPointCursor [ 1 ] 1) (mkPointCursor [ 1 ] 0) (mkCursor [ 1 ] 0 2 [] 0 0 (Inner Start))
    describe "basic zippers" do
      let e = fromTreeToTermSpan [ L "a", B [ L "b", L "c", L "d" ], L "e" ]
      do
        e # it_should_dragFromPointCursor "outer start to inner start " (mkPointCursor [] 1) (mkPointCursor [ 1 ] 0) (mkCursor [] 1 1 [ 1 ] 0 0 (Inner Start))
        e # it_should_dragFromPointCursor "outer start far to inner start " (mkPointCursor [] 0) (mkPointCursor [ 1 ] 0) (mkCursor [] 0 1 [ 1 ] 0 0 (Inner Start))
        e # it_should_dragFromPointCursor "outer start to inner start far " (mkPointCursor [] 1) (mkPointCursor [ 1 ] 1) (mkCursor [] 1 1 [ 1 ] 1 0 (Inner Start))
      do
        e # it_should_dragFromPointCursor "inner start to outer start " (mkPointCursor [ 1 ] 0) (mkPointCursor [] 1) (mkCursor [] 1 1 [ 0 ] 0 3 (Outer Start))
        e # it_should_dragFromPointCursor "inner start far to outer start " (mkPointCursor [ 1 ] 1) (mkPointCursor [] 1) (mkCursor [] 1 1 [ 0 ] 1 2 (Outer Start))
        e # it_should_dragFromPointCursor "inner start to outer start far " (mkPointCursor [ 1 ] 0) (mkPointCursor [] 0) (mkCursor [] 0 1 [ 1 ] 0 3 (Outer Start))
      do
        e # it_should_dragFromPointCursor "inner end to outer end " (mkPointCursor [ 1 ] 3) (mkPointCursor [] 2) (mkCursor [] 1 1 [ 1 ] 3 0 (Outer End))
        e # it_should_dragFromPointCursor "inner end to outer end far " (mkPointCursor [ 1 ] 3) (mkPointCursor [] 2) (mkCursor [] 1 1 [ 1 ] 3 0 (Outer End))
        pending "inner end far to outer end "
      do
        e # it_should_dragFromPointCursor "outer end to inner end " (mkPointCursor [] 2) (mkPointCursor [ 1 ] 3) (mkCursor [] 1 1 [ 1 ] 0 0 (Inner End))
        e # it_should_dragFromPointCursor "outer end far to inner end " (mkPointCursor [] 3) (mkPointCursor [ 1 ] 3) (mkCursor [] 1 0 [ 1 ] 0 0 (Inner End))
        pending "outer end to inner end far "
  pure unit

--------------------------------------------------------------------------------

shouldEqual :: forall m a. Applicative m => Eq a => MonadThrow Error m => Show a => a -> a -> m Unit
shouldEqual a b = unless (a == b) $ fail $ show a <> " ≠\n  " <> show b <> "\n"

shouldEqualCursor :: forall m. Applicative m => MonadThrow Error m => TermSpan -> Cursor -> Cursor -> m Unit
shouldEqualCursor (Span es) c1 c2 = unless (c1 == c2) $ fail $ indent 2 $ "\n" <> prettyTermWithCursor c1 (Sexp {} es) <> "\n≠\n" <> prettyTermWithCursor c2 (Sexp {} es) <> "\n"

it_should_dragFromPointCursor :: String -> PointCursor -> PointCursor -> Cursor -> TermSpan -> Spec Unit
it_should_dragFromPointCursor s p1 p2 c e = it (s <> prettyPointCursor p1 <> " --> " <> prettyPointCursor p2) do e # should_dragFromCursor (fromPointCursorToCursor p1 e) p2 c

should_dragFromPointCursor :: forall m. MonadThrow Error m => PointCursor -> PointCursor -> Cursor -> TermSpan -> m Unit
should_dragFromPointCursor p1 p2 c e = should_dragFromCursor (fromPointCursorToCursor p1 e) p2 c e

should_dragFromCursor :: forall m. MonadThrow Error m => Cursor -> PointCursor -> Cursor -> TermSpan -> m Unit
should_dragFromCursor c1 p2 c e@(Span es) =
  let
    c' = dragFromCursor c1 p2 e
  in
    unless (c' == c) do
      fail $ indent 2 $ "\n" <> Array.foldMap (_ <> "\n")
        [ "drag from\n"
        , indent 4 $ prettyTermWithCursor c1 (Sexp {} es)
        , "\nto\n"
        , indent 4 $ prettyTermWithCursor (fromPointCursorToCursor p2 e) (Sexp {} es)
        , "\nis supposed to be\n"
        , indent 4 $ prettyTermWithCursor c (Sexp {} es)
        , "\nbut actually is\n"
        , indent 4 $ prettyTermWithCursor c' (Sexp {} es)
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

