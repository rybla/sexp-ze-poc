module Sexpze.Component.State where

import Prelude

import Data.Array as Array
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Debug as Debug
import Sexpze.Utility (bug, todo)

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type State =
  { cursor :: Cursor
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

data Zipper = Zipper Span Span

--------------------------------------------------------------------------------
-- drag
--------------------------------------------------------------------------------

dragFromPoint :: Point -> Point -> Span -> Cursor
dragFromPoint ps_ pe_ e =
  let
    ps /\ pe = if ps_ < pe_ then ps_ /\ pe_ else pe_ /\ ps_
    s = e # atSpanCursor (SpanCursor ps pe)
    { unopened, unclosed } = s # countUnopenedAndUnclosed
  in
    if unopened > 0 && unclosed > 0 then
      -- ==> span that needs to expand out to nearest valid parent
      todo "" {}
    else if unopened > 0 then
      -- ==> zipper with second half on the left
      let
        i = getPointRightAfterNthPrevUnclosedParenStartingFromPoint unopened pe s
      in
        MakeZipperCursor $ ZipperCursor ps pe i (i + one)
    else if unclosed > 0 then
      -- ==> zipper with second half on the right
      let
        i = getPointRightBeforeNthNextUnopenedParenStartingFromPoint unclosed pe s
      in
        MakeZipperCursor $ ZipperCursor ps pe i (i + one)
    else
      -- unopened == unclosed == 0
      -- ==> span
      MakeSpanCursor $ SpanCursor ps pe

getPointRightBeforeNthNextUnopenedParenStartingFromPoint :: Int -> Point -> Span -> Point
getPointRightBeforeNthNextUnopenedParenStartingFromPoint n0 p0 xs = go n0 p0
  where
  go n p = case xs # atIndex (getIndexRightAfterPoint p) of
    Lit _ -> go n (p + one)
    Open -> go (n + one) (p + one)
    Close | n == 1 -> p
    Close -> go (n - one) (p + one)

getPointRightAfterNthPrevUnclosedParenStartingFromPoint :: Int -> Point -> Span -> Point
getPointRightAfterNthPrevUnclosedParenStartingFromPoint n0 p0 xs = go n0 p0
  where
  go n p = case xs # atIndex (getIndexRightBeforePoint p) of
    Lit _ -> go n (p - one)
    Close -> go (n + one) (p - one)
    Open | n == 1 -> p
    Open -> go (n - one) (p - one)

countUnopenedAndUnclosed :: Span -> { unopened :: Int, unclosed :: Int }
countUnopenedAndUnclosed (Span xs) = go 0 0 xs
  where
  go unopened unclosed = Array.uncons >>> case _ of
    Nothing -> { unopened, unclosed }
    Just { head: Lit _, tail: xs' } -> xs' # go unopened unclosed
    Just { head: Open, tail: xs' } -> xs' # go unopened (unclosed + 1)
    Just { head: Close, tail: xs' } -> xs' # if unclosed > 0 then go unopened (unclosed - 1) else go (unopened + 1) unclosed

--------------------------------------------------------------------------------
-- utilities
--------------------------------------------------------------------------------

getIndexRightBeforePoint :: Point -> Index
getIndexRightBeforePoint i = wrap (unwrap i - 1)

getIndexRightAfterPoint :: Point -> Index
getIndexRightAfterPoint i = wrap (unwrap i)

atIndex :: Index -> Span -> Atom
atIndex (Index i) (Span es) = es Array.!! i # fromMaybe' (\_ -> bug "atIndex" ("index out of bounds: " <> show { i, es }))

beforeIndex :: Index -> Span -> Span
beforeIndex _ _ = todo "" {}

afterIndex :: Index -> Span -> Span
afterIndex _ _ = todo "" {}

atSpanCursor :: SpanCursor -> Span -> Span
atSpanCursor _ _ = todo "" {}

atZipperCursor :: SpanCursor -> Span -> Zipper
atZipperCursor _ _ = todo "" {}
