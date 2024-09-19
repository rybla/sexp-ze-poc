module Test.Cursor where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array as Array
import Data.Newtype (wrap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Exception (Error)
import Sexpze.Data.Sexp (Sexp(..))
import Sexpze.Data.Sexp.Cursor (Cursor(..), PointCursor, Span(..), SpanHandle(..), Zipper(..), ZipperHandle(..), atSpanCursor, atSpanPointIndexSpan, deleteAtCursor, fromPointCursorToCursor, insertAtCursor)
import Sexpze.Data.Sexp.Cursor.Drag (dragFromCursor)
import Sexpze.Data.Sexp.Cursor.Pretty (indent, prettyPointCursor, prettyTermWithCursor)
import Test.Common (TermSpan, mkCursor, mkPointCursor, mkSpan, mkSpanCursor, shouldEqual)
import Test.Cursor.Drag as Drag
import Test.Spec (Spec, describe, it)
import Test.Tree (Tree(..), fromTreeToTermSpan)

spec :: Spec Unit
spec = describe "Cursor" do
  Drag.spec
  delete_spec
  insert_spec

delete_spec :: Spec Unit
delete_spec = describe "delete" do
  describe "basic" do
    describe "atSpanPointIndexSpan" do
      it "delete empty span" do
        let e = fromTreeToTermSpan [ L "a" ]
        let w /\ e' = atSpanPointIndexSpan (wrap 0) (wrap 0) e
        shouldEqual
          (w (fromTreeToTermSpan [ L "•" ]) /\ e')
          (fromTreeToTermSpan [ L "•" ] /\ fromTreeToTermSpan [ L "a" ])
      it "delete small span" do
        let e = fromTreeToTermSpan [ L "a" ]
        let w /\ e' = atSpanPointIndexSpan (wrap 1) (wrap 0) e
        shouldEqual
          (w (fromTreeToTermSpan [ L "•" ]) /\ e')
          (fromTreeToTermSpan [ L "a", L "•" ] /\ fromTreeToTermSpan [])
    describe "atSpanCursor" do
      it "delete empty span" do
        let e = fromTreeToTermSpan [ L "a" ]
        let w /\ e' = atSpanCursor (mkSpanCursor [] 0 0) e
        shouldEqual
          (w (fromTreeToTermSpan [ L "•" ]) /\ e')
          (fromTreeToTermSpan [ L "•" ] /\ (fromTreeToTermSpan [ L "a" ]))
      it "delete small span" do
        let e = fromTreeToTermSpan [ L "a" ]
        let w /\ e' = atSpanCursor (mkSpanCursor [] 1 0) e
        shouldEqual
          (w (fromTreeToTermSpan [ L "•" ]) /\ e')
          (fromTreeToTermSpan [ L "a", L "•" ] /\ (fromTreeToTermSpan []))
    describe "deleteAtCursor" do
      it "delete small span" $ do
        let e = fromTreeToTermSpan [ L "a" ]
        let e' = fromTreeToTermSpan []
        shouldEqual
          (deleteAtCursor (mkCursor [] 0 0 [] 1 0 (Inner Start)) e)
          (mkCursor [] 0 0 [] 0 0 (Inner Start) /\ e')
    describe "insertAtCursor" do
      it "insert small span" $ do
        let e = fromTreeToTermSpan []
        let e' = fromTreeToTermSpan [ L "a" ]
        shouldEqual
          (insertAtCursor (Zipper (fromTreeToTermSpan [ L "a" ]) (mkPointCursor mempty 0)) (mkCursor [] 0 0 [] 0 0 (Inner Start)) e)
          (mkCursor [] 0 1 [] 0 0 (Inner Start) /\ e')
  pure unit

insert_spec :: Spec Unit
insert_spec = describe "insert" do
  pure unit
