module Sexpze.Component.State where

import Prelude

import Data.Array as Array
import Data.Array as String
import Data.Eq.Generic (genericEq)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Sexpze.Utility (bug, todo)

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

newtype Point = Point Int

derive instance Newtype Point _
derive newtype instance Show Point
derive newtype instance Eq Point
derive newtype instance Ord Point
derive newtype instance Semiring Point
derive newtype instance Ring Point

newtype Index = Index Int

derive instance Newtype Index _
derive newtype instance Show Index
derive newtype instance Eq Index
derive newtype instance Ord Index
derive newtype instance Semiring Index
derive newtype instance Ring Index

data SpanCursor = SpanCursor Point Point

derive instance Generic SpanCursor _

instance Show SpanCursor where
  show x = genericShow x

instance Eq SpanCursor where
  eq x = genericEq x

endpointLeft :: SpanCursor -> Point
endpointLeft (SpanCursor p _) = p

endpointRight :: SpanCursor -> Point
endpointRight (SpanCursor _ p) = p

data ZipperCursor = ZipperCursor Point Point Point Point

derive instance Generic ZipperCursor _

instance Show ZipperCursor where
  show x = genericShow x

instance Eq ZipperCursor where
  eq x = genericEq x

data Cursor
  = MakeSpanCursor SpanCursor
  | MakeZipperCursor ZipperCursor

derive instance Generic Cursor _

endpointOfZipperCursor :: ZipperCursorOrientation -> ZipperCursor -> Point
endpointOfZipperCursor (Outer Start) (ZipperCursor pol _pil _pir _por) = pol
endpointOfZipperCursor (Inner Start) (ZipperCursor _pol _pil pir _por) = pir
endpointOfZipperCursor (Inner End) (ZipperCursor _pol pil _pir _por) = pil
endpointOfZipperCursor (Outer End) (ZipperCursor _pol _pil _pir por) = por

endpointOuterLeft :: ZipperCursor -> Point
endpointOuterLeft (ZipperCursor p _ _ _) = p

endpointInnerLeft :: ZipperCursor -> Point
endpointInnerLeft (ZipperCursor _ p _ _) = p

endpointInnerRight :: ZipperCursor -> Point
endpointInnerRight (ZipperCursor _ _ p _) = p

endpointOuterRight :: ZipperCursor -> Point
endpointOuterRight (ZipperCursor _ _ _ p) = p

instance Show Cursor where
  show x = genericShow x

instance Eq Cursor where
  eq x = genericEq x

data Clipboard
  = SpanClipboard Span
  | ZipperClipboard Zipper

derive instance Generic Clipboard _

instance Show Clipboard where
  show x = genericShow x

instance Eq Clipboard where
  eq x = genericEq x

data Atom = Lit String | Open | Close

derive instance Generic Atom _

instance Show Atom where
  show x = genericShow x

instance Eq Atom where
  eq x = genericEq x

newtype Span = Span (Array Atom)

derive instance Newtype Span _
derive newtype instance Show Span
derive newtype instance Eq Span
derive newtype instance Semigroup Span
derive newtype instance Monoid Span

length :: Span -> Int
length (Span es) = Array.length es

data Zipper = Zipper Span Span

derive instance Generic Zipper _

instance Show Zipper where
  show x = genericShow x

instance Eq Zipper where
  eq x = genericEq x

instance Semigroup Zipper where
  append (Zipper ol or) (Zipper il ir) = Zipper (ol <> il) (ir <> or)

instance Monoid Zipper where
  mempty = Zipper mempty mempty

lengthLeft :: Zipper -> Int
lengthLeft (Zipper ls _) = length ls

lengthRight :: Zipper -> Int
lengthRight (Zipper _ rs) = length rs

-- TODO: remove marker (Maybe SpanCursor)
-- TODO: add orientation (which point is "focus") for sake of keyboard movement
data CursorState
  = SpanCursorState SpanCursor SpanCursorOrientation
  | ZipperCursorState ZipperCursor ZipperCursorOrientation

derive instance Generic CursorState _

instance Show CursorState where
  show x = genericShow x

instance Eq CursorState where
  eq x = genericEq x

data SpanCursorOrientation
  = Start
  | End

derive instance Generic SpanCursorOrientation _

instance Show SpanCursorOrientation where
  show x = genericShow x

instance Eq SpanCursorOrientation where
  eq x = genericEq x

data ZipperCursorOrientation
  = Inner SpanCursorOrientation
  | Outer SpanCursorOrientation

derive instance Generic ZipperCursorOrientation _

instance Show ZipperCursorOrientation where
  show x = genericShow x

instance Eq ZipperCursorOrientation where
  eq x = genericEq x

--------------------------------------------------------------------------------
-- make
--------------------------------------------------------------------------------

makeSpanCursor :: Point -> Point -> Cursor
makeSpanCursor pl pr = MakeSpanCursor $ SpanCursor pl pr

makeZipperCursor :: Point -> Point -> Point -> Point -> Cursor
makeZipperCursor pol pil pir por = MakeZipperCursor $ ZipperCursor pol pil pir por

makePointCursor :: Point -> Cursor
makePointCursor p = MakeSpanCursor $ SpanCursor p p

--------------------------------------------------------------------------------
-- pretty
--------------------------------------------------------------------------------

prettySpan :: Span -> String
prettySpan = unwrap >>> map prettyAtom >>> String.intercalate " "

prettyAtom :: Atom -> String
prettyAtom (Lit str) = str
prettyAtom Open = "("
prettyAtom Close = ")"

--------------------------------------------------------------------------------
-- makeSpanFromDrag
--------------------------------------------------------------------------------

-- this is well-tested!
makeSpanCursorFromDrag :: Point -> Point -> Span -> SpanCursor
makeSpanCursorFromDrag p1 p2 e =
  let
    pl /\ pr = if p1 <= p2 then p1 /\ p2 else p2 /\ p1
    e' = e # atSpanCursor (SpanCursor pl pr) # snd
    { unopened, unclosed } = e' # countUnopenedAndUnclosedParens
    pl' =
      if unopened == 0 then pl
      else
        -- need to expand left the number of unopened parens
        getPointRightBeforeNthPrevUnclosedParenStartingFromPoint unopened pl e
    pr' =
      if unclosed == 0 then pr
      else
        -- need to expand right the number of unclosed parens
        getPointRightAfterNthNextUnopenedParenStartingFromPoint unclosed pr e
  in
    SpanCursor pl' pr'

--------------------------------------------------------------------------------
-- makeZipperCursorFromSpanCursors
--------------------------------------------------------------------------------

-- | Assumes the two spans are valid
makeZipperCursorFromSpanCursors :: SpanCursor -> SpanCursor -> Span -> ZipperCursor
makeZipperCursorFromSpanCursors (SpanCursor pol por) (SpanCursor pil pir) _e | pol <= pil && pir <= por = ZipperCursor pol pil pir por
makeZipperCursorFromSpanCursors (SpanCursor pil pir) (SpanCursor pol por) _e | pol <= pil && pir <= por = ZipperCursor pol pil pir por
makeZipperCursorFromSpanCursors (SpanCursor pol pir) (SpanCursor pil por) _e | pol <= pil && pir <= por = ZipperCursor pol pil pir por
makeZipperCursorFromSpanCursors (SpanCursor pil por) (SpanCursor pol pir) _e | pol <= pil && pir <= por = ZipperCursor pol pil pir por
makeZipperCursorFromSpanCursors (SpanCursor _pl1 _pr1) (SpanCursor _pl2 _pr2) _e = bug "makeZipperCursorFromSpanCursors" "impossible"

--------------------------------------------------------------------------------
-- dragFromPoint
--------------------------------------------------------------------------------

dragFromPoint :: Point -> Point -> Span -> Cursor
dragFromPoint p1_top p2_top e =
  let
    pl_top /\ pr_top = if p1_top < p2_top then p1_top /\ p2_top else p2_top /\ p1_top
    e' = e # atSpanCursor (SpanCursor pl_top pr_top) # snd
    { unopened, unclosed } = e' # countUnopenedAndUnclosedParens
  in
    if unopened > 0 && unclosed > 0 then
      -- ==> span that needs to expand out to nearest valid parent
      let
        pl = getPointRightAfterNthPrevUnclosedParenStartingFromPoint 1 pl_top e - one
        pr = getPointRightBeforeNthNextUnopenedParenStartingFromPoint 1 pr_top e + one
      in
        MakeSpanCursor $ SpanCursor pl pr
    else if unopened > 0 then
      -- ==> zippr_topr with second half on the left
      let
        pol = getPointRightAfterNthPrevUnclosedParenStartingFromPoint unopened pl_top e - one
        pil = getPointRightAfterNthPrevUnclosedParenStartingFromPoint 1 pl_top e
        pir = pl_top
        por = pr_top
      in
        MakeZipperCursor $ ZipperCursor pol pil pir por
    else if unclosed > 0 then
      -- ==> zippr_topr with second half on the right
      let
        pol = pl_top
        pil = pr_top
        pir = getPointRightBeforeNthNextUnopenedParenStartingFromPoint unclosed pr_top e
        por = getPointRightBeforeNthNextUnopenedParenStartingFromPoint unopened pr_top e + one
      in
        MakeZipperCursor $ ZipperCursor pol pil pir por
    else
      -- unopened == unclosed == 0 ==> span
      MakeSpanCursor $ SpanCursor pl_top pr_top

-- | looks to the right
getPointRightAfterNthNextUnopenedParenStartingFromPoint :: Int -> Point -> Span -> Point
getPointRightAfterNthNextUnopenedParenStartingFromPoint n0 p0 xs = go n0 p0
  where
  go n p = case xs # atIndex (getIndexRightAfterPoint p) of
    Lit _ -> go n (p + one)
    Open -> go (n + one) (p + one)
    Close | n == 1 -> p + one
    Close -> go (n - one) (p + one)

-- | looks to the right
getPointRightBeforeNthNextUnopenedParenStartingFromPoint :: Int -> Point -> Span -> Point
getPointRightBeforeNthNextUnopenedParenStartingFromPoint n0 p0 xs = go n0 p0
  where
  go n p = case xs # atIndex (getIndexRightAfterPoint p) of
    Lit _ -> go n (p + one)
    Open -> go (n + one) (p + one)
    Close | n == 1 -> p
    Close -> go (n - one) (p + one)

-- | looks to the left
getPointRightBeforeNthPrevUnclosedParenStartingFromPoint :: Int -> Point -> Span -> Point
getPointRightBeforeNthPrevUnclosedParenStartingFromPoint n0 p0 xs = go n0 p0
  where
  go n p = case xs # atIndex (getIndexRightBeforePoint p) of
    Lit _ -> go n (p - one)
    Close -> go (n + one) (p - one)
    Open | n == 1 -> p - one
    Open -> go (n - one) (p - one)

-- | looks to the left
getPointRightAfterNthPrevUnclosedParenStartingFromPoint :: Int -> Point -> Span -> Point
getPointRightAfterNthPrevUnclosedParenStartingFromPoint n0 p0 xs = go n0 p0
  where
  go n p = case xs # atIndex (getIndexRightBeforePoint p) of
    Lit _ -> go n (p - one)
    Close -> go (n + one) (p - one)
    Open | n == 1 -> p
    Open -> go (n - one) (p - one)

countUnopenedAndUnclosedParens :: Span -> { unopened :: Int, unclosed :: Int }
countUnopenedAndUnclosedParens (Span xs) = go 0 0 xs
  where
  go unopened unclosed = Array.uncons >>> case _ of
    Nothing -> { unopened, unclosed }
    Just { head: Lit _, tail: xs' } -> xs' # go unopened unclosed
    Just { head: Open, tail: xs' } -> xs' # go unopened (unclosed + 1)
    Just { head: Close, tail: xs' } -> xs' # if unclosed > 0 then go unopened (unclosed - 1) else go (unopened + 1) unclosed

--------------------------------------------------------------------------------
-- interact with SpanCursos and ZipperCursor
--------------------------------------------------------------------------------

deleteAtSpanCursor :: SpanCursor /\ Span -> SpanCursor /\ Span
deleteAtSpanCursor = replaceAtSpanCursor mempty

deleteAtZipperCursor :: ZipperCursor /\ Span -> SpanCursor /\ Span
deleteAtZipperCursor = replaceAtZipperCursor mempty

insertAtSpanCursor :: Zipper -> SpanCursor /\ Span -> SpanCursor /\ Span
insertAtSpanCursor z (c /\ e) = replaceAtZipperCursor z (fromSpanCursorToEmptyZipperCursor c /\ e)

replaceAtSpanCursor :: Zipper -> SpanCursor /\ Span -> SpanCursor /\ Span
replaceAtSpanCursor z (c /\ e) =
  replaceAtZipperCursor z (fromSpanCursorToFullZipperCursor c /\ e)

replaceAtZipperCursor :: Zipper -> ZipperCursor /\ Span -> SpanCursor /\ Span
replaceAtZipperCursor z (c@(ZipperCursor pol pil pir _por) /\ e) =
  Tuple
    (SpanCursor (pol # shiftPoint (lengthLeft z)) (pol # shiftPoint (lengthLeft z + distBetweenPoints pil pir)))
    (e # atZipperCursor c # fst # (_ $ atZipper z))

--------------------------------------------------------------------------------
-- utilities
--------------------------------------------------------------------------------

getIndexRightBeforePoint :: Point -> Index
getIndexRightBeforePoint i = wrap (unwrap i - 1)

getIndexRightAfterPoint :: Point -> Index
getIndexRightAfterPoint i = wrap (unwrap i)

atIndex :: Index -> Span -> Atom
atIndex i s = unwrap s Array.!! unwrap i # fromMaybe' (\_ -> bug "atIndex" ("index out of bounds: " <> show { i, s }))

beforePoint :: Point -> Span -> Span
beforePoint p s = unwrap s # Array.take (unwrap p) # wrap

afterPoint :: Point -> Span -> Span
afterPoint p s = unwrap s # Array.drop (unwrap p) # wrap

atSpanCursor :: SpanCursor -> Span -> Tuple (Span -> Span) Span
atSpanCursor (SpanCursor pl pr) e =
  Tuple
    (\es' -> beforePoint pl e <> es' <> afterPoint pr e)
    (e # unwrap # Array.slice (unwrap pl) (unwrap pr) # wrap)

atZipperCursor :: ZipperCursor -> Span -> Tuple ((Span -> Span) -> Span) Zipper
atZipperCursor (ZipperCursor pol pil pir por) s =
  let
    to_pol = s # atSpanCursor (SpanCursor (wrap 0) pol) # snd
    por_to = s # atSpanCursor (SpanCursor por (wrap (length s))) # snd
    s_pol_to_pil = s # atSpanCursor (SpanCursor pol pil) # snd
    s_pir_to_por = s # atSpanCursor (SpanCursor pir por) # snd
  in
    Tuple
      (\w -> to_pol <> w (s # atSpanCursor (SpanCursor pil pir) # snd) <> por_to)
      (Zipper s_pol_to_pil s_pir_to_por)

atZipper :: Zipper -> (Span -> Span)
atZipper (Zipper sl sr) sm = sl <> sm <> sr

fromSpanCursorToEmptyZipperCursor :: SpanCursor -> ZipperCursor
fromSpanCursorToEmptyZipperCursor (SpanCursor pl pr) = ZipperCursor pl pl pr pr

fromSpanCursorToFullZipperCursor :: SpanCursor -> ZipperCursor
fromSpanCursorToFullZipperCursor (SpanCursor pl pr) = ZipperCursor pl pr pr pr

foldMapPointsAndWithIndex :: forall a m. Monoid m => (Point -> m) -> (Index -> a -> m) -> Array a -> m
foldMapPointsAndWithIndex f_point f_index xs =
  Array.snoc
    (mapWithIndex (\i x -> f_point (wrap i) <> f_index (wrap i) x) xs)
    (f_point (xs # Array.length # wrap))
    # Array.fold

shiftPoint :: Int -> Point -> Point
shiftPoint dx (Point x) = Point (dx + x)

distBetweenPoints :: Point -> Point -> Int
distBetweenPoints (Point x) (Point y) | x <= y = y - x
distBetweenPoints (Point x) (Point y) = bug "distBetweenPoints" $ "required: " <> show x <> " <= " <> show y

ltPointAndIndex :: Point -> Index -> Boolean
ltPointAndIndex p i = unwrap p <= unwrap i

gtPointAndIndex :: Point -> Index -> Boolean
gtPointAndIndex p i = unwrap p >= unwrap i

ltIndexAndPoint :: Index -> Point -> Boolean
ltIndexAndPoint i p = unwrap i < unwrap p

gtIndexAndPoint :: Index -> Point -> Boolean
gtIndexAndPoint i p = unwrap i > unwrap p

fromCursorStateToEmptySpanCursor :: CursorState -> SpanCursor
fromCursorStateToEmptySpanCursor (SpanCursorState c o) = let i = endpointLeft c in SpanCursor i i
fromCursorStateToEmptySpanCursor (ZipperCursorState c o) = let i = endpointInnerLeft c in SpanCursor i i

fromCursorStateToPoint :: CursorState -> Point
fromCursorStateToPoint (SpanCursorState c o) = endpointLeft c
fromCursorStateToPoint (ZipperCursorState c o) = endpointInnerLeft c

fromZipperCursorWithOrientationToSpanCursorWithOrientation :: ZipperCursor /\ ZipperCursorOrientation -> SpanCursor /\ SpanCursorOrientation
fromZipperCursorWithOrientationToSpanCursorWithOrientation (c /\ (Outer Start)) = (SpanCursor (endpointOfZipperCursor (Outer Start) c) (endpointOfZipperCursor (Outer End) c) /\ Start)
fromZipperCursorWithOrientationToSpanCursorWithOrientation (c /\ (Inner Start)) = (SpanCursor (endpointOfZipperCursor (Inner Start) c) (endpointOfZipperCursor (Inner End) c) /\ Start)
fromZipperCursorWithOrientationToSpanCursorWithOrientation (c /\ (Inner End)) = (SpanCursor (endpointOfZipperCursor (Inner Start) c) (endpointOfZipperCursor (Inner End) c) /\ End)
fromZipperCursorWithOrientationToSpanCursorWithOrientation (c /\ (Outer End)) = (SpanCursor (endpointOfZipperCursor (Outer Start) c) (endpointOfZipperCursor (Outer End) c) /\ End)
