module Sexpze.Data.Sexp.Cursor.Pretty where

import Prelude
import Sexpze.Data.Sexp
import Sexpze.Data.Sexp.Cursor

import Data.Array (mapWithIndex, (!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (type (\/), either5)
import Data.Foldable (maximum)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromMaybe, fromMaybe')
import Data.Newtype (wrap)
import Data.Show.Generic (genericShow)
import Data.String (codePointFromChar)
import Data.String as String
import Data.String.CodePoints (codePointFromChar)
import Data.Tuple.Nested ((/\))
import Sexpze.Utility (bug, todo)

--------------------------------------------------------------------------------

type Term = Sexp NodeData AtomData
type Term' = Sexp' NodeData AtomData
type TermSpan = Span NodeData AtomData
type TermZipper = Zipper NodeData AtomData

type NodeData = {}
type AtomData = String

defaultNodeData :: NodeData
defaultNodeData = {}

--------------------------------------------------------------------------------

type Items = Array Item
data Item = Item (Array String)

derive instance Generic Item _

instance Show Item where
  show x = genericShow x

prettyItems :: Items -> String
prettyItems items =
  items
    # Array.foldr
        prettyItem'
        (Array.replicate max_row_count "")
    # Array.intercalate "\n"
  where
  max_row_count = items # map (\(Item row) -> Array.length row) # maximum # fromMaybe 0

prettyItem' :: Item -> Array String -> Array String
prettyItem' (Item col) =
  mapWithIndex \i ->
    ((col !! i # fromMaybe "" # padRight max_row_length) <> _)
  where
  max_row_length = col # map String.length # maximum # fromMaybe 0

padRight :: Int -> String -> String
padRight n s = let i = n - String.length s in if i > 0 then s <> String.fromCodePointArray (Array.replicate i (String.codePointFromChar ' ')) else s

indent :: Int -> String -> String
indent n = String.split (String.Pattern "\n") >>> map (indentation <> _) >>> Array.intercalate "\n"
  where
  indentation = String.fromCodePointArray (Array.replicate n (String.codePointFromChar ' '))

--------------------------------------------------------------------------------

prettyTermWithCursor :: Cursor -> Term -> String
prettyTermWithCursor (Cursor c h) = renderTermWithCursor (Left c) h mempty >>> prettyItems

prettyTerm :: Term -> String
prettyTerm = renderTerm mempty >>> prettyItems

renderTermWithCursor :: ZipperCursor \/ SpanCursor -> ZipperHandle -> Path -> Term -> Items
renderTermWithCursor c h ph (Sexp _n es) =
  let
    e = Span es
  in
    unconsZipperCursor c
      # either5
          ( \(_i /\ c') ->
              es
                # mapWithPointIndex
                    (\j' -> [ renderPoint (PointCursor ph j') ])
                    (\i' -> renderTerm'WithCursor (Left c') h ph i')
                # Array.fold
          )
          ( \((d1_outer /\ d2_inner) /\ (_i /\ c')) ->
              let
                j1_outer = shiftPointIndexByPointDist d1_outer (wrap 0)
                j2_outer = shiftPointIndexByPointDistNeg d2_inner (lastPointIndexOfSpan e)
              in
                es
                  # mapWithPointIndex
                      ( \j' ->
                          [ if j' == j1_outer then [ renderZipperHandle h (Outer Start) (PointCursor ph j1_outer) ] else []
                          , if j' == j2_outer then [ renderZipperHandle h (Outer End) (PointCursor ph j2_outer) ] else []
                          , if not $ j' == j1_outer || j' == j2_outer then [ renderPoint (PointCursor ph j') ] else []
                          ] # Array.fold
                      )
                      (\i' -> renderTerm'WithCursor (Right c') h ph i')
                  # Array.fold
          )
          ( \((d1_outer /\ d2_outer) /\ (d1_inner /\ d2_inner)) ->
              let
                j1_outer = shiftPointIndexByPointDist d1_outer (wrap 0)
                j2_outer = shiftPointIndexByPointDistNeg d2_outer (lastPointIndexOfSpan e)
                j1_inner = shiftPointIndexByPointDist (d1_outer + d1_inner) (wrap 0)
                j2_inner = shiftPointIndexByPointDistNeg (d2_outer + d2_inner) (lastPointIndexOfSpan e)
              in
                es
                  # mapWithPointIndex
                      ( \j' ->
                          [ if j' == j1_outer then [ renderZipperHandle h (Outer Start) (PointCursor ph j1_outer) ] else []
                          , if j' == j1_inner then [ renderZipperHandle h (Inner Start) (PointCursor ph j1_inner) ] else []
                          , if j' == j2_inner then [ renderZipperHandle h (Inner End) (PointCursor ph j2_inner) ] else []
                          , if j' == j2_outer then [ renderZipperHandle h (Outer End) (PointCursor ph j2_outer) ] else []
                          , if not $ j' == j1_inner || j' == j1_outer || j' == j2_inner || j' == j2_outer then [ renderPoint (PointCursor ph j') ] else []
                          ] # Array.fold
                      )
                      (renderTerm' ph)
                  # Array.fold
          )
          ( \(_i /\ c') ->
              es
                # mapWithPointIndex
                    (\j' -> [ renderPoint (PointCursor ph j') ])
                    (\i' -> renderTerm'WithCursor (Right c') h ph i')
                # Array.fold
          )
          ( \(d1_inner /\ d2_inner) ->
              let
                j1_inner = shiftPointIndexByPointDist d1_inner (wrap 0)
                j2_inner = shiftPointIndexByPointDistNeg d2_inner (lastPointIndexOfSpan e)
              in
                es
                  # mapWithPointIndex
                      ( \j' ->
                          [ if j' == j1_inner then [ renderZipperHandle h (Inner Start) (PointCursor ph j1_inner) ] else []
                          , if j' == j2_inner then [ renderZipperHandle h (Inner End) (PointCursor ph j2_inner) ] else []
                          , if not $ j' == j1_inner || j' == j2_inner then [ renderPoint (PointCursor ph j') ] else []
                          ] # Array.fold
                      )
                      (renderTerm' ph)
                  # Array.fold
          )

renderPoint :: PointCursor -> Item
renderPoint _ = Item [ " " ]

renderTerm' :: Path -> KidIndex -> Term' -> Items
renderTerm' ph i (Group e) =
  [ [ Item [ "(" ] ]
  , renderTerm (ph `snocPath` i) e
  , [ Item [ ")" ] ]
  ]
    # Array.fold
renderTerm' _ph _i (Atom a) =
  [ Item [ a ] ]

renderTerm :: Path -> Term -> Items
renderTerm ph (Sexp _ es) =
  es
    # mapWithPointIndex
        (\j' -> [ renderPoint (PointCursor ph j') ])
        (\i' -> renderTerm' ph i')
    # Array.fold

renderTerm'WithCursor :: ZipperCursor \/ SpanCursor -> ZipperHandle -> Path -> KidIndex -> Term' -> Items
renderTerm'WithCursor c h ph i (Group e) =
  [ [ Item [ "(" ] ]
  , renderTermWithCursor c h (ph `snocPath` i) e
  , [ Item [ ")" ] ]
  ]
    # Array.fold
renderTerm'WithCursor _c _h _ph _i (Atom a) =
  [ Item [ a ] ]

renderZipperHandle :: ZipperHandle -> ZipperHandle -> PointCursor -> Item
renderZipperHandle h h'@(Inner Start) _p = Item $ [ [ "[" ], if h == h' then [ "*" ] else [] ] # Array.fold
renderZipperHandle h h'@(Inner End) _p = Item $ [ [ "]" ], if h == h' then [ "*" ] else [] ] # Array.fold
renderZipperHandle h h'@(Outer Start) _p = Item $ [ [ "{" ], if h == h' then [ "*" ] else [] ] # Array.fold
renderZipperHandle h h'@(Outer End) _p = Item $ [ [ "}" ], if h == h' then [ "*" ] else [] ] # Array.fold

renderZipperHandleInnerMiddle :: ZipperHandle -> PointCursor -> Item
renderZipperHandleInnerMiddle h _ = case h of
  Outer _ -> Item [ "|" ]
  Inner _ -> Item [ "|", "*" ]

renderPointHandle :: PointCursor -> Item
renderPointHandle _ = Item [ "|", "*" ]
