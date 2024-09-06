module Sexpze.Component.App where

import Prelude

import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Sexpze.Component.Editor as Editor
import Type.Proxy (Proxy(..))

data Output = EditorOutput Editor.Output

component :: forall query input output. H.Component query input output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState _input = {}

  eval = H.mkEval H.defaultEval

  render _state = HH.div []
    [ HH.slot (Proxy :: Proxy "editor") unit Editor.component (Editor.Input {}) EditorOutput
    ]
