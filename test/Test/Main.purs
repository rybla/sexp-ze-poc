module Test.Main where

import Prelude

import Effect (Effect)
import Test.Cursor as Cursor
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Cursor.spec

-- main :: Effect Unit
-- main = do
--   when false do
--     let
--       cursor = PointCursor (Point (0 : Nil) 0)
--       subcursor = cursor /\ PointSubCursorStatus
--       term = [ (Group [ (Atom "a"), (Atom "b") ]) ]

--     Console.log $ show { cursor }
--     Console.log $ show { subcursor }
--     Console.log $ show { term }

--     let mb_subcursor' = matchStepSubCursor 0 subcursor
--     Console.log $ show { mb_subcursor' }

--     let mb_cs = toCursorStatus 0 =<< mb_subcursor'
--     Console.log $ show { mb_cs }

--   when false do
--     let
--       cursor = SpanCursor (Span (Point (0 : Nil) 0) (Point (0 : Nil) 1))
--       subcursor = cursor /\ PointSubCursorStatus
--       term = [ (Group [ (Atom "a"), (Atom "b") ]) ]

--     Console.log $ show { cursor }
--     Console.log $ show { subcursor }
--     Console.log $ show { term }

--     let mb_subcursor' = matchStepSubCursor 0 subcursor
--     Console.log $ show { mb_subcursor' }

--     Console.log $ show { mb_cs1: toCursorStatus 0 =<< mb_subcursor' }
--     Console.log $ show { mb_cs2: toCursorStatus 1 =<< mb_subcursor' }
--     Console.log $ show { mb_cs3: toCursorStatus 2 =<< mb_subcursor' }

--   when true do
--     let cursor = ZipperCursor (Zipper (Point Nil 0) (Point (0 : Nil) 0) (Point (0 : Nil) 1) (Point Nil 1))
--     let subcursor = cursor /\ PointSubCursorStatus
--     let term = [ Group [ Atom "a" ] ]
--     Console.log $ show { cursor }
--     Console.log $ show { term }

--     when false do
--       Console.log $ show (matchStepSubCursor 0 subcursor)
--       Console.log $ show (toCursorStatus 0 subcursor)
--       Console.log $ show (toCursorStatus 0 <$> matchStepSubCursor 0 subcursor)
--       Console.log $ show (toCursorStatus 1 <$> matchStepSubCursor 0 subcursor)
--       Console.log $ show (toCursorStatus 1 subcursor)

--     Console.log $ show
--       ( traverseSexpWithCursor
--           { atom: \s -> s
--           , group: \{} ss -> " ( " <> ss <> " ) "
--           , list: \xs { last } ->
--               (xs <#> \{ before, r } -> show { status: before.status } <> r) <> [ show { status: last.status } ]
--                 # Array.intercalate " "
--           }
--           mempty
--           (pure subcursor)
--           term :: String
--       )

--     pure unit

--   pure unit

