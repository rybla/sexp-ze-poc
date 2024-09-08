module Test.Main where

import Prelude
import Sexpze.Data.Sexp
import Sexpze.Data.Sexp.Cursor

import Data.List (List(..), (:))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console as Console

main :: Effect Unit
main = do
  when false do
    let
      cursor = PointCursor (Point (0 : Nil) 0)
      subcursor = cursor /\ PointSubCursorStatus
      term = [ (Group [ (Atom "a"), (Atom "b") ]) ]

    Console.log $ show { cursor }
    Console.log $ show { subcursor }
    Console.log $ show { term }

    let mb_subcursor' = matchStepSubCursor 0 subcursor
    Console.log $ show { mb_subcursor' }

    let mb_cs = toCursorStatus 0 =<< mb_subcursor'
    Console.log $ show { mb_cs }

  when true do
    let
      cursor = SpanCursor (Span { p0: Point (0 : Nil) 0, p1: Point (0 : Nil) 1 })
      subcursor = cursor /\ PointSubCursorStatus
      term = [ (Group [ (Atom "a"), (Atom "b") ]) ]

    Console.log $ show { cursor }
    Console.log $ show { subcursor }
    Console.log $ show { term }

    let mb_subcursor' = matchStepSubCursor 0 subcursor
    Console.log $ show { mb_subcursor' }

    Console.log $ show { mb_cs1: toCursorStatus 0 =<< mb_subcursor' }
    Console.log $ show { mb_cs2: toCursorStatus 1 =<< mb_subcursor' }
    Console.log $ show { mb_cs3: toCursorStatus 2 =<< mb_subcursor' }

  pure unit

