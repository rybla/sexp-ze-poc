module Sexpze.Component.Editor where

import Prelude

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Sexpze.Data.Tree (Tree(..))
import Sexpze.Utility (todo)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

--------------------------------------------------------------------------------
-- component
--------------------------------------------------------------------------------

data Query a = KeyboardEvent_Query KeyboardEvent a

newtype Input = Input {}

data Output = Updated Term

type Slots :: Row Type
type Slots = ()

type M = Aff

component :: H.Component Query Input Output M
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState _input =
    { term: Tree "init" []
    }

  eval = H.mkEval H.defaultEval { handleAction = handleAction }

  handleAction = case _ of
    UserAction_Action a -> handleUserAction a

  render _state =
    HH.div
      []
      [ HH.text "Editor.component" ]

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

type State =
  { term :: Term
  }

type Term = Tree Label
type Label = String

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
  | Delete
  | Copy
  | Paste

parseUserActionsFromKeyboardEvent :: KeyboardEvent -> State -> Maybe (Array UserAction)
parseUserActionsFromKeyboardEvent ke {} =
  if key == "ArrowLeft" && shift then
    pure [ SelectLeft ]
  else if key == "ArrowRight" && shift then
    pure [ SelectRight ]
  else if key == "ArrowLeft" then
    pure [ MoveLeft ]
  else if key == "ArrowRight" then
    pure [ MoveRight ]
  else if key == "Backspace" then
    pure [ Delete ]
  else if key == "c" && cmd then
    pure [ Copy ]
  else if key == "v" && cmd then
    pure [ Paste ]
  else if key == "x" && cmd then
    pure [ Copy, Delete ]
  else empty
  where
  key = KeyboardEvent.key ke
  shift = KeyboardEvent.shiftKey ke
  cmd = KeyboardEvent.ctrlKey ke || KeyboardEvent.metaKey ke

-- parseUserActionsFromMouseClick :: TODO -> State -> Maybe (Array UserAction)
-- parseUserActionsFromMouseClick = todo "parseUserActionsFromMouseClick" {}

--------------------------------------------------------------------------------
-- Action
--------------------------------------------------------------------------------

data Action =
  UserAction_Action UserAction

--------------------------------------------------------------------------------
-- Logic
--------------------------------------------------------------------------------

handleUserAction :: UserAction -> H.HalogenM State Action Slots Output M Unit
handleUserAction = todo "doUserAction" {}
