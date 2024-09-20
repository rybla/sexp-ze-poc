{-
idea:
- cursor is a span
- by default, an empty span (looks like ibeam)
  - on keyboard, can expand span left or right with a special key
  - on mouse, can expand span by just dragging a new one, or dragging out endpoints of existing span
- selection is between two spans: first navigate cursor (span) to a position, set mark
  - on keyboard, hold shift and make a new cursor (span) by moving and expanding left/right
  - on mouse, hold shift, and make a new cursor (span) by dragging
-}

module Sexpze.Component.State where

import Prelude

import Data.Array as Array
import Data.Array as String
import Data.Eq.Generic (genericEq)
import Data.Foldable (length)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Sexpze.Utility (bug, todo)

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type State =
  { cursor :: Cursor
  , span :: Span
  , mb_clipboard :: Maybe Clipboard
  }

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

instance Show Cursor where
  show x = genericShow x

instance Eq Cursor where
  eq x = genericEq x

data Clipboard
  = SpanClipboard Span
  | ZipperClipboard

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

data Zipper = Zipper Span Span

--------------------------------------------------------------------------------
-- make
--------------------------------------------------------------------------------

makeSpanCursor :: Point -> Point -> Cursor
makeSpanCursor pl pr = MakeSpanCursor $ SpanCursor pl pr

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
-- insert
--------------------------------------------------------------------------------

insertAroundSpanCursor :: Zipper -> SpanCursor -> Span -> Span
insertAroundSpanCursor z c = replaceAtZipperCursor z (fromSpanCursorToEmptyZipperCursor c)

replaceAtSpanCursor :: Zipper -> SpanCursor -> Span -> Span
replaceAtSpanCursor z c = replaceAtZipperCursor z (fromSpanCursorToFullZipperCursor c)

replaceAtZipperCursor :: Zipper -> ZipperCursor -> Span -> Span
replaceAtZipperCursor z c = atZipperCursor c >>> fst >>> (_ $ atZipper z)

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
  Tuple
    (todo "" {})
    (Zipper (s # atSpanCursor (SpanCursor pol pil) # snd) (s # atSpanCursor (SpanCursor pir por) # snd))

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
    (f_point (xs # length))
    # Array.fold
