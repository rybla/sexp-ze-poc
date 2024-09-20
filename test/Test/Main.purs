module Test.Main where

import Prelude
import Sexpze.Component.State

import Data.Newtype (wrap)
import Data.String as String
import Effect (Effect)
import Test.Spec (Spec, describe)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Utility (it_shouldEqual)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  spec_getPointRightBeforeNthNextUnopenedParenStartingFromPoint
  spec_getPointRightAfterNthPrevUnclosedParenStartingFromPoint
  spec_dragFromPoint
  spec_makeSpanCursorFromDrag

spec_getPointRightBeforeNthNextUnopenedParenStartingFromPoint :: Spec Unit
spec_getPointRightBeforeNthNextUnopenedParenStartingFromPoint = describe "getPointRightBeforeNthNextUnopenedParenStartingFromPoint" do
  let
    it_shouldEqual_etc { actual: { n, p0, e }, expected: { p1 } } = do
      it_shouldEqual (show p0 <> " --> ... " <> show n <> " unopened  in  " <> prettySpan e) { actual: getPointRightBeforeNthNextUnopenedParenStartingFromPoint n p0 e, expected: p1 }
  describe "simple" do
    let e = "( a b c )" # parseSpan
    it_shouldEqual_etc { actual: { n: 1, p0: wrap 1, e }, expected: { p1: wrap 4 } }
    it_shouldEqual_etc { actual: { n: 1, p0: wrap 2, e }, expected: { p1: wrap 4 } }
    it_shouldEqual_etc { actual: { n: 1, p0: wrap 3, e }, expected: { p1: wrap 4 } }
  describe "advanced" do
    let e = "( a ( b ( c ) d ) e )" # parseSpan
    it_shouldEqual_etc { actual: { n: 1, p0: wrap 5, e }, expected: { p1: wrap 6 } }
    it_shouldEqual_etc { actual: { n: 2, p0: wrap 5, e }, expected: { p1: wrap 8 } }
    it_shouldEqual_etc { actual: { n: 3, p0: wrap 5, e }, expected: { p1: wrap 10 } }
  describe "tricky" do
    let e = "( a )" # parseSpan
    it_shouldEqual_etc { actual: { n: 1, p0: wrap 1, e }, expected: { p1: wrap 2 } }

spec_getPointRightAfterNthPrevUnclosedParenStartingFromPoint :: Spec Unit
spec_getPointRightAfterNthPrevUnclosedParenStartingFromPoint = describe "getPointRightAfterNthPrevUnclosedParenStartingFromPoint" do
  let
    it_shouldEqual_etc { actual: { n, p0, e }, expected: { p1 } } = do
      it_shouldEqual (show n <> " unclosed ... <-- " <> show p0 <> "  in  " <> prettySpan e) { actual: getPointRightAfterNthPrevUnclosedParenStartingFromPoint n p0 e, expected: p1 }
  describe "simple" do
    let e = "( a b c )" # parseSpan
    it_shouldEqual_etc { actual: { n: 1, p0: wrap 1, e }, expected: { p1: wrap 1 } }
    it_shouldEqual_etc { actual: { n: 1, p0: wrap 2, e }, expected: { p1: wrap 1 } }
    it_shouldEqual_etc { actual: { n: 1, p0: wrap 3, e }, expected: { p1: wrap 1 } }
  describe "advanced" do
    let e = "( a ( b ( c ) d ) e )" # parseSpan
    it_shouldEqual_etc { actual: { n: 1, p0: wrap 5, e }, expected: { p1: wrap 5 } }
    it_shouldEqual_etc { actual: { n: 2, p0: wrap 5, e }, expected: { p1: wrap 3 } }
    it_shouldEqual_etc { actual: { n: 3, p0: wrap 5, e }, expected: { p1: wrap 1 } }
  describe "tricky" do
    let e = "( a )" # parseSpan
    it_shouldEqual_etc { actual: { n: 1, p0: wrap 2, e }, expected: { p1: wrap 1 } }

spec_dragFromPoint :: Spec Unit
spec_dragFromPoint = describe "dragFromPoint" do
  let
    it_shouldEqual_dragFromPoint { actual: { pl, pr, e }, expected } = do
      it_shouldEqual (show pl <> " --> " <> show pr <> "  in  " <> prettySpan e) { actual: dragFromPoint pl pr e, expected }
      it_shouldEqual (show pl <> " <-- " <> show pr <> "  in  " <> prettySpan e) { actual: dragFromPoint pr pl e, expected }
  describe "span" do
    describe "flat" do
      let e = "a b c" # parseSpan
      it_shouldEqual_dragFromPoint { actual: { pl: wrap 0, pr: wrap 1, e }, expected: makeSpanCursor (wrap 0) (wrap 1) }
      it_shouldEqual_dragFromPoint { actual: { pl: wrap 0, pr: wrap 2, e }, expected: makeSpanCursor (wrap 0) (wrap 2) }
      it_shouldEqual_dragFromPoint { actual: { pl: wrap 0, pr: wrap 3, e }, expected: makeSpanCursor (wrap 0) (wrap 3) }
      it_shouldEqual_dragFromPoint { actual: { pl: wrap 1, pr: wrap 2, e }, expected: makeSpanCursor (wrap 1) (wrap 2) }
      it_shouldEqual_dragFromPoint { actual: { pl: wrap 1, pr: wrap 3, e }, expected: makeSpanCursor (wrap 1) (wrap 3) }
      it_shouldEqual_dragFromPoint { actual: { pl: wrap 3, pr: wrap 3, e }, expected: makeSpanCursor (wrap 3) (wrap 3) }
    describe "deeper" do
      let e = "a ( b c ) d" # parseSpan
      it_shouldEqual_dragFromPoint { actual: { pl: wrap 0, pr: wrap 1, e }, expected: makeSpanCursor (wrap 0) (wrap 1) }
      it_shouldEqual_dragFromPoint { actual: { pl: wrap 2, pr: wrap 3, e }, expected: makeSpanCursor (wrap 2) (wrap 3) }
      it_shouldEqual_dragFromPoint { actual: { pl: wrap 2, pr: wrap 4, e }, expected: makeSpanCursor (wrap 2) (wrap 4) }
      it_shouldEqual_dragFromPoint { actual: { pl: wrap 5, pr: wrap 6, e }, expected: makeSpanCursor (wrap 5) (wrap 6) }
      it_shouldEqual_dragFromPoint { actual: { pl: wrap 0, pr: wrap 5, e }, expected: makeSpanCursor (wrap 0) (wrap 5) }
      it_shouldEqual_dragFromPoint { actual: { pl: wrap 1, pr: wrap 5, e }, expected: makeSpanCursor (wrap 1) (wrap 5) }
    describe "jumpt to outer" do
      let e = "( a ) ( b )" # parseSpan
      it_shouldEqual_dragFromPoint { actual: { pl: wrap 2, pr: wrap 4, e }, expected: makeSpanCursor (wrap 0) (wrap 6) }
  describe "zipper" do
    describe "simple" do
      let e = "a ( b ) c" # parseSpan
      -- it_shouldEqual_dragFromPoint { actual: { pl: wrap 2, pr: wrap 4, e }, expected: makeSpanCursor (wrap 0) (wrap 6) }
      pure unit

--------------------------------------------------------------------------------

spec_makeSpanCursorFromDrag :: Spec Unit
spec_makeSpanCursorFromDrag = describe "makeSpanCursorFromDrag" do
  let my_it { p1, p2, e, c } = it_shouldEqual (show p1 <> " --> " <> show p2 <> "  in  " <> prettySpan e) { actual: makeSpanCursorFromDrag p1 p2 e, expected: c }
  do
    let e = "a ( b ( c ) d ) e" # parseSpan
    -- starting at 0
    my_it { p1: wrap 0, p2: wrap 0, e, c: SpanCursor (wrap 0) (wrap 0) }
    my_it { p1: wrap 0, p2: wrap 1, e, c: SpanCursor (wrap 0) (wrap 1) }
    my_it { p1: wrap 0, p2: wrap 2, e, c: SpanCursor (wrap 0) (wrap 8) }
    my_it { p1: wrap 0, p2: wrap 3, e, c: SpanCursor (wrap 0) (wrap 8) }
    my_it { p1: wrap 0, p2: wrap 4, e, c: SpanCursor (wrap 0) (wrap 8) }
    my_it { p1: wrap 0, p2: wrap 5, e, c: SpanCursor (wrap 0) (wrap 8) }
    my_it { p1: wrap 0, p2: wrap 6, e, c: SpanCursor (wrap 0) (wrap 8) }
    my_it { p1: wrap 0, p2: wrap 7, e, c: SpanCursor (wrap 0) (wrap 8) }
    my_it { p1: wrap 0, p2: wrap 8, e, c: SpanCursor (wrap 0) (wrap 8) }
    my_it { p1: wrap 0, p2: wrap 9, e, c: SpanCursor (wrap 0) (wrap 9) }
    -- starting at 1
    my_it { p1: wrap 1, p2: wrap 1, e, c: SpanCursor (wrap 1) (wrap 1) }
    my_it { p1: wrap 1, p2: wrap 2, e, c: SpanCursor (wrap 1) (wrap 8) }
    my_it { p1: wrap 1, p2: wrap 3, e, c: SpanCursor (wrap 1) (wrap 8) }
    my_it { p1: wrap 1, p2: wrap 4, e, c: SpanCursor (wrap 1) (wrap 8) }
    my_it { p1: wrap 1, p2: wrap 5, e, c: SpanCursor (wrap 1) (wrap 8) }
    my_it { p1: wrap 1, p2: wrap 6, e, c: SpanCursor (wrap 1) (wrap 8) }
    my_it { p1: wrap 1, p2: wrap 7, e, c: SpanCursor (wrap 1) (wrap 8) }
    my_it { p1: wrap 1, p2: wrap 8, e, c: SpanCursor (wrap 1) (wrap 8) }
    my_it { p1: wrap 1, p2: wrap 9, e, c: SpanCursor (wrap 1) (wrap 9) }
    -- starting at 2
    my_it { p1: wrap 2, p2: wrap 2, e, c: SpanCursor (wrap 2) (wrap 2) }
    my_it { p1: wrap 2, p2: wrap 3, e, c: SpanCursor (wrap 2) (wrap 3) }
    my_it { p1: wrap 2, p2: wrap 4, e, c: SpanCursor (wrap 2) (wrap 6) }
    my_it { p1: wrap 2, p2: wrap 5, e, c: SpanCursor (wrap 2) (wrap 6) }
    my_it { p1: wrap 2, p2: wrap 6, e, c: SpanCursor (wrap 2) (wrap 6) }
    my_it { p1: wrap 2, p2: wrap 7, e, c: SpanCursor (wrap 2) (wrap 7) }
    my_it { p1: wrap 2, p2: wrap 8, e, c: SpanCursor (wrap 1) (wrap 8) }
    my_it { p1: wrap 2, p2: wrap 9, e, c: SpanCursor (wrap 1) (wrap 9) }
    -- starting at 3
    my_it { p1: wrap 3, p2: wrap 3, e, c: SpanCursor (wrap 3) (wrap 3) }
    my_it { p1: wrap 3, p2: wrap 4, e, c: SpanCursor (wrap 3) (wrap 6) }
    my_it { p1: wrap 3, p2: wrap 5, e, c: SpanCursor (wrap 3) (wrap 6) }
    my_it { p1: wrap 3, p2: wrap 6, e, c: SpanCursor (wrap 3) (wrap 6) }
    my_it { p1: wrap 3, p2: wrap 7, e, c: SpanCursor (wrap 3) (wrap 7) }
    my_it { p1: wrap 3, p2: wrap 8, e, c: SpanCursor (wrap 1) (wrap 8) }
    my_it { p1: wrap 3, p2: wrap 9, e, c: SpanCursor (wrap 1) (wrap 9) }
    -- starting at 4
    my_it { p1: wrap 4, p2: wrap 4, e, c: SpanCursor (wrap 4) (wrap 4) }
    my_it { p1: wrap 4, p2: wrap 5, e, c: SpanCursor (wrap 4) (wrap 5) }
    my_it { p1: wrap 4, p2: wrap 6, e, c: SpanCursor (wrap 3) (wrap 6) }
    my_it { p1: wrap 4, p2: wrap 7, e, c: SpanCursor (wrap 3) (wrap 7) }
    my_it { p1: wrap 4, p2: wrap 8, e, c: SpanCursor (wrap 1) (wrap 8) }
    my_it { p1: wrap 4, p2: wrap 9, e, c: SpanCursor (wrap 1) (wrap 9) }
    -- starting at 5
    my_it { p1: wrap 5, p2: wrap 5, e, c: SpanCursor (wrap 5) (wrap 5) }
    my_it { p1: wrap 5, p2: wrap 6, e, c: SpanCursor (wrap 3) (wrap 6) }
    my_it { p1: wrap 5, p2: wrap 7, e, c: SpanCursor (wrap 3) (wrap 7) }
    my_it { p1: wrap 5, p2: wrap 8, e, c: SpanCursor (wrap 1) (wrap 8) }
    my_it { p1: wrap 5, p2: wrap 9, e, c: SpanCursor (wrap 1) (wrap 9) }
    -- starting at 6
    my_it { p1: wrap 6, p2: wrap 6, e, c: SpanCursor (wrap 6) (wrap 6) }
    my_it { p1: wrap 6, p2: wrap 7, e, c: SpanCursor (wrap 6) (wrap 7) }
    my_it { p1: wrap 6, p2: wrap 8, e, c: SpanCursor (wrap 1) (wrap 8) }
    my_it { p1: wrap 6, p2: wrap 9, e, c: SpanCursor (wrap 1) (wrap 9) }
    -- starting at 7
    my_it { p1: wrap 7, p2: wrap 7, e, c: SpanCursor (wrap 7) (wrap 7) }
    my_it { p1: wrap 7, p2: wrap 8, e, c: SpanCursor (wrap 1) (wrap 8) }
    my_it { p1: wrap 7, p2: wrap 9, e, c: SpanCursor (wrap 1) (wrap 9) }
    -- starting at 8
    my_it { p1: wrap 8, p2: wrap 8, e, c: SpanCursor (wrap 8) (wrap 8) }
    my_it { p1: wrap 8, p2: wrap 9, e, c: SpanCursor (wrap 8) (wrap 9) }
    -- starting at 9
    my_it { p1: wrap 9, p2: wrap 9, e, c: SpanCursor (wrap 9) (wrap 9) }

parseSpan :: String -> Span
parseSpan = String.split (String.Pattern " ") >>> map parseAtom >>> Span
  where
  parseAtom "(" = Open
  parseAtom ")" = Close
  parseAtom s = Lit s

