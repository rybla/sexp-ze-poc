module Test.Cursor.Drag where

import Prelude

import Test.Common (TermSpan, mkCursor, mkPointCursor)
import Test.Tree (Tree(..), fromTreeToTermSpan)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array as Array
import Effect.Exception (Error)
import Sexpze.Data.Sexp (Sexp(..))
import Sexpze.Data.Sexp.Cursor (Cursor, PointCursor, Span(..), SpanHandle(..), ZipperHandle(..), fromPointCursorToCursor)
import Sexpze.Data.Sexp.Cursor.Drag (dragFromCursor)
import Sexpze.Data.Sexp.Cursor.Pretty (indent, prettyPointCursor, prettyTermWithCursor)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

spec :: Spec Unit
spec = do
  describe "dragFromPointCursor" do
    points_spec
    spans_spec
    zippers_spec

points_spec :: Spec Unit
points_spec = describe "points" do
  describe "basic" do
    let e = fromTreeToTermSpan [ L "a", L "b", L "c" ]
    e # it_should_dragFromPointCursor "" (mkPointCursor [] 0) (mkPointCursor [] 0) (mkCursor [] 0 3 [] 0 0 (Inner Start))
    e # it_should_dragFromPointCursor "" (mkPointCursor [] 1) (mkPointCursor [] 1) (mkCursor [] 1 2 [] 0 0 (Inner Start))
    e # it_should_dragFromPointCursor "" (mkPointCursor [] 2) (mkPointCursor [] 2) (mkCursor [] 2 1 [] 0 0 (Inner Start))
    e # it_should_dragFromPointCursor "" (mkPointCursor [] 3) (mkPointCursor [] 3) (mkCursor [] 3 0 [] 0 0 (Inner Start))
  describe "deeper" do
    let e = fromTreeToTermSpan [ L "a", B [ L "b", L "c", L "d" ], L "e" ]
    e # it_should_dragFromPointCursor "" (mkPointCursor [ 1 ] 0) (mkPointCursor [ 1 ] 0) (mkCursor [ 1 ] 0 3 [] 0 0 (Inner Start))
    e # it_should_dragFromPointCursor "" (mkPointCursor [ 1 ] 1) (mkPointCursor [ 1 ] 1) (mkCursor [ 1 ] 1 2 [] 0 0 (Inner Start))
    e # it_should_dragFromPointCursor "" (mkPointCursor [ 1 ] 2) (mkPointCursor [ 1 ] 2) (mkCursor [ 1 ] 2 1 [] 0 0 (Inner Start))
    e # it_should_dragFromPointCursor "" (mkPointCursor [ 1 ] 3) (mkPointCursor [ 1 ] 3) (mkCursor [ 1 ] 3 0 [] 0 0 (Inner Start))

spans_spec :: Spec Unit
spans_spec = describe "spans" do
  describe "basic" do
    let e = fromTreeToTermSpan [ L "a", L "b", L "c" ]
    e # it_should_dragFromPointCursor "to end " (mkPointCursor [] 0) (mkPointCursor [] 1) (mkCursor [] 0 2 [] 1 0 (Inner Start))
    e # it_should_dragFromPointCursor "to start " (mkPointCursor [] 1) (mkPointCursor [] 0) (mkCursor [] 0 2 [] 1 0 (Inner Start))
  describe "deeper" do
    let e = fromTreeToTermSpan [ L "a", B [ L "b", L "c", L "d" ], L "e" ]
    e # it_should_dragFromPointCursor "start to end " (mkPointCursor [ 1 ] 0) (mkPointCursor [ 1 ] 1) (mkCursor [ 1 ] 0 2 [] 1 0 (Inner Start))
    e # it_should_dragFromPointCursor "end to start " (mkPointCursor [ 1 ] 1) (mkPointCursor [ 1 ] 0) (mkCursor [ 1 ] 0 2 [] 1 0 (Inner Start))

zippers_spec :: Spec Unit
zippers_spec = describe "zippers" do
  describe "basic" do
    let e = fromTreeToTermSpan [ L "a", B [ L "b", L "c", L "d" ], L "e" ]
    do
      e # it_should_dragFromPointCursor "outer start to inner start " (mkPointCursor [] 1) (mkPointCursor [ 1 ] 0) (mkCursor [] 1 1 [ 0 ] 0 0 (Inner Start))
      e # it_should_dragFromPointCursor "outer start far to inner start " (mkPointCursor [] 0) (mkPointCursor [ 1 ] 0) (mkCursor [] 0 1 [ 1 ] 0 0 (Inner Start))
      e # it_should_dragFromPointCursor "outer start to inner start far " (mkPointCursor [] 1) (mkPointCursor [ 1 ] 1) (mkCursor [] 1 1 [ 0 ] 1 0 (Inner Start))
    do
      e # it_should_dragFromPointCursor "inner start to outer start " (mkPointCursor [ 1 ] 0) (mkPointCursor [] 1) (mkCursor [] 1 1 [ 0 ] 0 3 (Outer Start))
      e # it_should_dragFromPointCursor "inner start far to outer start " (mkPointCursor [ 1 ] 1) (mkPointCursor [] 1) (mkCursor [] 1 1 [ 0 ] 1 2 (Outer Start))
      e # it_should_dragFromPointCursor "inner start to outer start far " (mkPointCursor [ 1 ] 0) (mkPointCursor [] 0) (mkCursor [] 0 1 [ 1 ] 0 3 (Outer Start))
    do
      e # it_should_dragFromPointCursor "inner end to outer end " (mkPointCursor [ 1 ] 3) (mkPointCursor [] 2) (mkCursor [] 1 1 [ 0 ] 3 0 (Outer End))
      e # it_should_dragFromPointCursor "inner end to outer end far " (mkPointCursor [ 1 ] 3) (mkPointCursor [] 2) (mkCursor [] 1 1 [ 0 ] 3 0 (Outer End))
      e # it_should_dragFromPointCursor "inner end far to outer end " (mkPointCursor [ 1 ] 2) (mkPointCursor [] 2) (mkCursor [] 1 1 [ 0 ] 2 1 (Outer End))
    do
      e # it_should_dragFromPointCursor "outer end to inner end " (mkPointCursor [] 2) (mkPointCursor [ 1 ] 3) (mkCursor [] 1 1 [ 0 ] 0 0 (Inner End))
      e # it_should_dragFromPointCursor "outer end far to inner end " (mkPointCursor [] 3) (mkPointCursor [ 1 ] 3) (mkCursor [] 1 0 [ 0 ] 0 0 (Inner End))
      e # it_should_dragFromPointCursor "outer end to inner end far " (mkPointCursor [] 2) (mkPointCursor [ 1 ] 2) (mkCursor [] 1 1 [ 0 ] 0 1 (Inner End))

--------------------------------------------------------------------------------

it_should_dragFromPointCursor :: String -> PointCursor -> PointCursor -> Cursor -> TermSpan -> Spec Unit
it_should_dragFromPointCursor s p1 p2 c e =
  it (s <> prettyPointCursor p1 <> " --> " <> prettyPointCursor p2) do
    e # should_dragFromCursor (fromPointCursorToCursor p1 e) p2 c

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
        , "\n"
        , show c' <> " ≠"
        , show c
        ]

