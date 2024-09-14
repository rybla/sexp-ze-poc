module Test.Main where

import Prelude

import Effect (Effect)
import Test.Cursor as Cursor
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Cursor.spec
