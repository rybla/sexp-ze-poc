module Test.Common where

import Prelude

import Data.List as List
import Data.Newtype (wrap)
import Sexpze.Data.Sexp (Sexp, Sexp')
import Sexpze.Data.Sexp.Cursor (Cursor(..), KidIndex, Path(..), PointCursor(..), Span(..), SpanCursor(..), ZipperCursor(..), ZipperHandle)
import Test.Spec.Assertions (fail)
import Test.Spec.Assertions as Assertions

type Term = Sexp {} String
type Term' = Sexp' {} String
type TermSpan = Span {} String

mkSpan :: Array Term' -> TermSpan
mkSpan = Span

mkCursor :: Array Int -> Int -> Int -> Array Int -> Int -> Int -> ZipperHandle -> Cursor
mkCursor ph_outer d1_outer d2_outer ph_inner d1_inner d2_inner h =
  Cursor
    ( ZipperCursor
        (mkSpanCursor ph_outer d1_outer d2_outer)
        (mkSpanCursor ph_inner d1_inner d2_inner)
    )
    h

mkSpanCursor :: Array Int -> Int -> Int -> SpanCursor
mkSpanCursor ph d1 d2 = SpanCursor (mkPath ph) (wrap d1) (wrap d2)

mkPath :: Array Int -> Path
mkPath is = is # map (wrap :: _ -> KidIndex) # List.fromFoldable # Path

mkPointCursor :: Array Int -> Int -> PointCursor
mkPointCursor is j = PointCursor (mkPath is) (wrap j)

--------------------------------------------------------------------------------

shouldEqual a b = unless (a == b) do fail $ show a <> " ≠\n  " <> show b <> "\n"

