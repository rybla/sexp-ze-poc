module Sexpze.Component.State where

import Prelude

import Data.Array as Array
import Data.Array as String
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Sexpze.Utility (bug)

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
-- drag
--------------------------------------------------------------------------------

dragFromPoint :: Point -> Point -> Span -> Cursor
dragFromPoint p1_top p2_top e =
  let
    pl_top /\ pr_top = if p1_top < p2_top then p1_top /\ p2_top else p2_top /\ p1_top
    e' = e # atSpanCursor (SpanCursor pl_top pr_top)
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
getPointRightBeforeNthNextUnopenedParenStartingFromPoint :: Int -> Point -> Span -> Point
getPointRightBeforeNthNextUnopenedParenStartingFromPoint n0 p0 xs = go n0 p0
  where
  go n p = case xs # atIndex (getIndexRightAfterPoint p) of
    Lit _ -> go n (p + one)
    Open -> go (n + one) (p + one)
    Close | n == 1 -> p
    Close -> go (n - one) (p + one)

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

atSpanCursor :: SpanCursor -> Span -> Span
atSpanCursor (SpanCursor pl pr) = unwrap >>> Array.slice (unwrap pl) (unwrap pr) >>> wrap

atZipperCursor :: ZipperCursor -> Span -> Zipper
atZipperCursor (ZipperCursor pol pil pir por) s = Zipper (s # atSpanCursor (SpanCursor pol pil)) (s # atSpanCursor (SpanCursor pir por))
