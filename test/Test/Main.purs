module Test.Main where

import Prelude
import Sexpze.Component.State

import Data.Newtype (wrap)
import Data.String as String
import Effect (Effect)
import Test.Spec (describe)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Utility (it_shouldEqual)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "getPointRightBeforeNthNextUnopenedParenStartingFromPoint" do
    describe "simple" do
      let e = "( a b c )" # parseSpan
      it_shouldEqual "1" { actual: getPointRightBeforeNthNextUnopenedParenStartingFromPoint 1 (wrap 1) e, expected: wrap 4 }
      it_shouldEqual "2" { actual: getPointRightBeforeNthNextUnopenedParenStartingFromPoint 1 (wrap 2) e, expected: wrap 4 }
      it_shouldEqual "3" { actual: getPointRightBeforeNthNextUnopenedParenStartingFromPoint 1 (wrap 3) e, expected: wrap 4 }
    describe "advanced" do
      let e = "( a ( b ( c ) d ) e )" # parseSpan
      it_shouldEqual "1" { actual: getPointRightBeforeNthNextUnopenedParenStartingFromPoint 1 (wrap 5) e, expected: wrap 6 }
      it_shouldEqual "2" { actual: getPointRightBeforeNthNextUnopenedParenStartingFromPoint 2 (wrap 5) e, expected: wrap 8 }
      it_shouldEqual "3" { actual: getPointRightBeforeNthNextUnopenedParenStartingFromPoint 3 (wrap 5) e, expected: wrap 10 }
    describe "tricky" do
      let e = "( a )" # parseSpan
      it_shouldEqual "1" { actual: getPointRightBeforeNthNextUnopenedParenStartingFromPoint 1 (wrap 1) e, expected: wrap 2 }
  describe "getPointRightAfterNthPrevUnclosedParenStartingFromPoint" do
    describe "simple" do
      let e = "( a b c )" # parseSpan
      it_shouldEqual "1" { actual: getPointRightAfterNthPrevUnclosedParenStartingFromPoint 1 (wrap 1) e, expected: wrap 1 }
      it_shouldEqual "2" { actual: getPointRightAfterNthPrevUnclosedParenStartingFromPoint 1 (wrap 2) e, expected: wrap 1 }
      it_shouldEqual "3" { actual: getPointRightAfterNthPrevUnclosedParenStartingFromPoint 1 (wrap 3) e, expected: wrap 1 }
    describe "advanced" do
      let e = "( a ( b ( c ) d ) e )" # parseSpan
      it_shouldEqual "1" { actual: getPointRightAfterNthPrevUnclosedParenStartingFromPoint 1 (wrap 5) e, expected: wrap 5 }
      it_shouldEqual "2" { actual: getPointRightAfterNthPrevUnclosedParenStartingFromPoint 2 (wrap 5) e, expected: wrap 3 }
      it_shouldEqual "3" { actual: getPointRightAfterNthPrevUnclosedParenStartingFromPoint 3 (wrap 5) e, expected: wrap 1 }
    describe "tricky" do
      let e = "( a )" # parseSpan
      it_shouldEqual "1" { actual: getPointRightAfterNthPrevUnclosedParenStartingFromPoint 1 (wrap 2) e, expected: wrap 1 }

--------------------------------------------------------------------------------

parseSpan :: String -> Span
parseSpan = String.split (String.Pattern " ") >>> map parseAtom >>> Span
  where
  parseAtom "(" = Open
  parseAtom ")" = Close
  parseAtom s = Lit s
