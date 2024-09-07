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

  pure unit

