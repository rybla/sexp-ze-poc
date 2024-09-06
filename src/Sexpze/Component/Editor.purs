module Sexpze.Component.Editor where

import Prelude

import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Sexpze.Data.Tree (Tree(..))
import Sexpze.Utility (todo)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

--------------------------------------------------------------------------------
-- component
--------------------------------------------------------------------------------

data Query a = KeyboardEvent_Query KeyboardEvent a

newtype Input = Input {}

data Output = Updated Term

component :: H.Component Query Input Output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState _input = { term: Tree "init" [] }

  eval = H.mkEval H.defaultEval

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

-- parseUserActionsFromKeyboardEvent :: KeyboardEvent -> State -> Maybe (Array UserAction)
-- parseUserActionsFromKeyboardEvent = todo "parseUserActionsFromKeyboardEvent" {}

-- parseUserActionsFromMouseClick :: TODO -> State -> Maybe (Array UserAction)
-- parseUserActionsFromMouseClick = todo "parseUserActionsFromMouseClick" {}

