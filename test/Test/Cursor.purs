module Test.Cursor where

import Prelude

import Test.Cursor.Drag as Drag
import Test.Spec (Spec, describe)

spec :: Spec Unit
spec = describe "Cursor" do
  Drag.spec

