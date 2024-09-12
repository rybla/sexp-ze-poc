module Sexpze.Component.Editor where

import Prelude
import Sexpze.Data.Sexp
import Sexpze.Data.Sexp.Cursor

import Control.Monad.State (get, modify_)
import Control.Plus (empty)
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sexpze.Utility (bug, todo)
import Web.Event.Event as Event
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type Term = Sexp String
type Term' = Sexp' String

--------------------------------------------------------------------------------
-- component
--------------------------------------------------------------------------------

data Query a = KeyboardEvent_Query KeyboardEvent a

newtype Input = Input
  { term :: Term
  , cursor :: Cursor
  }

data Output = Updated Term

type Slots :: Row Type
type Slots = ()

type M = Aff
type HM = H.HalogenM State Action Slots Output M

type HTML = H.ComponentHTML Action Slots M

component :: H.Component Query Input Output M
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState (Input input) =
    { term: input.term
    , cursor: input.cursor
    }

  eval = H.mkEval H.defaultEval

  render _state = HH.div [] [ HH.text "Editor" ]

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

type State =
  { term :: Term
  , cursor :: Cursor
  }

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
  | StartDrag Point
  | EndDrag Point
  | InsertAtom String
  | InsertGroup

derive instance Generic UserAction _

instance Show UserAction where
  show x = genericShow x

--------------------------------------------------------------------------------
-- Action
--------------------------------------------------------------------------------

data Action = UserAction_Action UserAction ActionConfig

derive instance Generic Action _

instance Show Action where
  show x = genericShow x

data ActionConfig = MouseActionConfig
  { event :: MouseEvent
  , doStopPropagation :: Boolean
  }

instance Show ActionConfig where
  show _ = "ActionConfig { ... }"
