module Test.Cursor.Drag where

import Prelude

import Data.List as List
import Data.Newtype (wrap)
import Sexpze.Data.Sexp (Sexp(..), Sexp'(..))
import Sexpze.Data.Sexp.Cursor (Cursor(..), KidIndex(..), Path(..), PointCursor(..), Span(..), SpanCursor(..), SpanHandle(..), ZipperCursor(..), ZipperHandle(..), emptyZipperCursor)
import Sexpze.Data.Sexp.Cursor.Drag (dragFromCursor)
import Sexpze.Utility (todo)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "basic" do
    it "point" do
      let e = Span [ Atom "a", Group (Sexp {} [ Atom "b" ]), Atom "c" ]
      shouldEqual
        ( dragFromCursor
            (mkCursor [] 0 3 [] 0 0 (Inner Start))
            (PointCursor mempty (wrap 0))
            e
        )
        (mkCursor [] 0 3 [] 0 0 (Inner Start))
      pure unit

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

