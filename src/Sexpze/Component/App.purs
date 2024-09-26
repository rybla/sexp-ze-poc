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
import Sexpze.Component.State (Atom(..), Cursor(..), Span(..), SpanCursor(..))
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.HTML as Web.HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Web.HTML.Window
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes as KET

data Action
  = Initialize
  | KeyDown_Action KeyboardEvent
  | KeyUp_Action KeyboardEvent
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
          KET.keydown
          (HTMLDocument.toEventTarget document)
          (map KeyDown_Action <<< KeyboardEvent.fromEvent)
      H.subscribe' \_ ->
        HQE.eventListener
          KET.keydown
          (HTMLDocument.toEventTarget document)
          (map KeyUp_Action <<< KeyboardEvent.fromEvent)

    KeyDown_Action ke -> do
      when (KeyboardEvent.key ke == " ") do
        ke # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
      H.tell (Proxy :: Proxy "editor") unit $ Editor.KeyDown_Query ke

    KeyUp_Action ke -> do
      when (KeyboardEvent.key ke == " ") do
        ke # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
      H.tell (Proxy :: Proxy "editor") unit $ Editor.KeyUp_Query ke

    EditorOutput _out -> do
      pure unit -- TODO

  render _state =
    HH.div []
      [ HH.slot (Proxy :: Proxy "editor") unit Editor.component
          -- { span: Span [ Lit "a", Open, Lit "b", Open, Lit "c", Close, Lit "d", Close, Lit "e" ] }
          -- { span: Span [ Lit "a", Lit "b", Lit "c", Lit "d", Lit "e", Lit "f", Lit "g", Lit "h", Lit "i" ] }
          { span: Span [ Open, Lit "f", Lit "x", Lit "y", Close ] }
          EditorOutput
      ]
