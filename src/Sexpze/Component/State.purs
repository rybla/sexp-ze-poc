module Sexpze.Component.State where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.Tuple.Nested ((/\))
import Sexpze.Utility (todo)

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type State =
  { cursor :: Cursor
  , mb_clipboard :: Maybe Clipboard
  }

newtype Index = Index Int

newtype PointCursor = PointCursor Int

derive instance Newtype PointCursor _
derive newtype instance Eq PointCursor
derive newtype instance Ord PointCursor
derive newtype instance Semiring PointCursor

data SpanCursor = SpanCursor PointCursor PointCursor

data ZipperCursor = ZipperCursor PointCursor PointCursor PointCursor PointCursor

data Cursor
  = MakeSpanCursor SpanCursor
  | MakeZipperCursor ZipperCursor

data Clipboard
  = SpanClipboard Span
  | ZipperClipboard

data Atom = LitAtom String | Open | Close

newtype Span = Span (Array Atom)

data Zipper = Zipper Span Span

--------------------------------------------------------------------------------
-- drag
--------------------------------------------------------------------------------

dragFromPointCursor :: PointCursor -> PointCursor -> Span -> Cursor
dragFromPointCursor ps pe e | ps == pe = MakeSpanCursor $ SpanCursor ps pe
dragFromPointCursor ps pe e =
  let
    s = e # atSpanCursor (SpanCursor ps pe)
    { unopened, unclosed } = s # countUnopenedAndUnclosed
  in
    if unopened > 0 && unclosed > 0 then
      -- ==> span that needs to expand out to nearest valid parent
      todo "" {}
    else if unopened > 0 then
      -- ==> zipper with second half on the left
      todo "" {}
    else if unclosed > 0 then
      -- ==> zipper with second half on the right
      todo "" {}
    else
      -- unopened == unclosed == 0
      -- ==> span
      todo "" {}

countUnopenedAndUnclosed :: Span -> { unopened :: Int, unclosed :: Int }
countUnopenedAndUnclosed (Span xs) = go 0 0 xs
  where
  go unopened unclosed = Array.uncons >>> case _ of
    Nothing -> { unopened, unclosed }
    Just { head: LitAtom _, tail: xs' } -> xs' # go unopened unclosed
    Just { head: Open, tail: xs' } -> xs' # go unopened (unclosed + 1)
    Just { head: Close, tail: xs' } -> xs' # if unclosed > 0 then go unopened (unclosed - 1) else go (unopened + 1) unclosed

--------------------------------------------------------------------------------
-- utilities
--------------------------------------------------------------------------------

atPointCursor :: PointCursor -> Span -> Atom
atPointCursor = todo ""

beforePointCursor :: PointCursor -> Span -> Span
beforePointCursor = todo "" {}

afterPointCursor :: PointCursor -> Span -> Span
afterPointCursor = todo "" {}

atSpanCursor :: SpanCursor -> Span -> Span
atSpanCursor = todo "" {}

atZipperCursor :: SpanCursor -> Span -> Zipper
atZipperCursor = todo "" {}
