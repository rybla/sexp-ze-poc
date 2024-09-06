module Sexpze.Component.Editor where

import Prelude

import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Sexpze.Utility (todo)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

--------------------------------------------------------------------------------
-- component
--------------------------------------------------------------------------------

data Query a = KeyboardEvent_Query KeyboardEvent a

data Input

data Output

type State = {}

component :: H.Component Query Input Output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState _input = {}

  eval = H.mkEval H.defaultEval

  render _state =
    HH.div
      []
      [ HH.text "Editor.component" ]

--------------------------------------------------------------------------------
-- UserAction
--
-- The basic actions that user input is parsed as a sequence of.
--------------------------------------------------------------------------------

data UserAction
  = MoveLeft
  | MoveRight
  | SelectLeft
  | SelectRight

parseUserActionsFromKeyboardEvent :: KeyboardEvent -> State -> Maybe (Array UserAction)
parseUserActionsFromKeyboardEvent = todo "parseUserActionsFromKeyboardEvent" {}

-- parseUserActionsFromMouseClick :: TODO -> State -> Maybe (Array UserAction)
-- parseUserActionsFromMouseClick = todo "parseUserActionsFromMouseClick" {}

