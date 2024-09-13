module Sexpze.Component.App where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.Event as HQE
import Sexpze.Component.Editor as Editor
import Sexpze.Data.Sexp (Sexp'(..))
import Sexpze.Data.Sexp.Cursor (Cursor(..), Point(..))
import Type.Proxy (Proxy(..))
import Web.HTML as Web.HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Web.HTML.Window
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes as KET

data Action
  = Initialize
  | KeyboardAction KeyboardEvent
  | EditorOutput Editor.Output

component :: forall query input output. H.Component query input output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState _input = {}

  eval = H.mkEval H.defaultEval { initialize = Just Initialize, handleAction = handleAction }

  handleAction = case _ of
    Initialize -> do
      document <- Web.HTML.window >>= Web.HTML.Window.document # liftEffect
      H.subscribe' \_ ->
        HQE.eventListener
          KET.keyup
          (HTMLDocument.toEventTarget document)
          (map KeyboardAction <<< KeyboardEvent.fromEvent)

    KeyboardAction ke -> do
      H.tell (Proxy :: Proxy "editor") unit $ Editor.KeyboardEvent_Query ke

    EditorOutput out -> do
      pure unit -- TODO

  render _state =
    HH.div []
      [ HH.slot (Proxy :: Proxy "editor") unit Editor.component
          ( Editor.Input
              { term: [ Group {} [ Atom "a", Atom "b", Atom "c" ] ]
              , cursor: InjectPoint (Point mempty (wrap 0))
              }
          )
          EditorOutput
      ]
