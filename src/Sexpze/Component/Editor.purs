module Sexpze.Component.Editor where

import Prelude
import Sexpze.Data.Sexp.Cursor
import Sexpze.Data.Sexp.Cursor.Drag

import Control.Monad.State (get, modify_)
import Control.Plus (empty)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (either5)
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, wrap)
import Data.Newtype as NT
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Debug as Debug
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sexpze.Data.Sexp (Sexp(..), Sexp'(..))
import Sexpze.Utility (todo)
import Web.Event.Event as Event
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.MouseEvent (MouseEvent)

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type Term = Sexp NodeData AtomData
type Term' = Sexp' NodeData AtomData
type TermSpan = Span NodeData AtomData
type TermZipper = Zipper NodeData AtomData

type NodeData = {}
type AtomData = { label :: String }

defaultNodeData :: NodeData
defaultNodeData = {}

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

type State =
  { term :: TermSpan
  , cursor :: Cursor
  , mb_clipboard :: Maybe Clipboard
  }

newtype Clipboard = Clipboard TermZipper

derive instance Newtype Clipboard _
derive newtype instance Show Clipboard
derive newtype instance Eq Clipboard

--------------------------------------------------------------------------------
-- component Types
--------------------------------------------------------------------------------

data Query a = KeyboardEvent_Query KeyboardEvent a

newtype Input = Input
  { term :: TermSpan
  , cursor :: Cursor
  }

data Output = Updated Term

type Slots :: Row Type
type Slots = ()

type M = Aff
type HM = H.HalogenM State Action Slots Output M

type HTML = H.ComponentHTML Action Slots M

--------------------------------------------------------------------------------
-- Action
--------------------------------------------------------------------------------

data Action
  = Initialize
  | UserAction_Action UserAction

--------------------------------------------------------------------------------
-- UserAction
--------------------------------------------------------------------------------

data UserAction
  = StartDrag_double PointCursor PointCursor
  | EndDrag_double PointCursor PointCursor
  | UserAction_Core UserAction_Core

derive instance Generic UserAction _

instance Show UserAction where
  show x = genericShow x

data UserAction_Core
  = Delete
  | Copy
  | Paste (Maybe Clipboard)
  | StartDrag PointCursor
  | EndDrag PointCursor

derive instance Generic UserAction_Core _

instance Show UserAction_Core where
  show x = genericShow x

handleUserAction :: UserAction -> HM Unit
handleUserAction = elaborateUserAction >=> handleUserAction_Core

elaborateUserAction :: UserAction -> HM UserAction_Core
elaborateUserAction action = todo "elaborateUserAction" { action }

handleUserAction_Core :: UserAction_Core -> HM Unit
handleUserAction_Core action = todo "handleUserAction_Core" { action }

--------------------------------------------------------------------------------
-- rendering
--------------------------------------------------------------------------------

renderTermWithCursor :: TermSpan -> Cursor -> Array HTML
renderTermWithCursor term cursor = todo "renderTermWithCursor" { term, cursor }

--------------------------------------------------------------------------------
-- component
--------------------------------------------------------------------------------

component :: H.Component Query Input Output M
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState (Input input) =
    { term: input.term
    , cursor: input.cursor
    , mb_clipboard: empty
    }

  eval = H.mkEval H.defaultEval { handleAction = handleAction, handleQuery = handleQuery }

  handleQuery :: forall a. Query a -> HM (Maybe a)
  handleQuery = case _ of
    KeyboardEvent_Query event a -> do
      Console.log $ "[KeyboardEvent_Query] " <> show { key: KeyboardEvent.key event }
      let actions = event # parseKeyboardEvent
      when (not (Array.null actions)) do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
      actions # traverse_ handleUserAction
      pure (Just a)

  handleAction :: Action -> HM Unit
  handleAction Initialize = pure unit
  handleAction (UserAction_Action action) = do
    handleUserAction action
    pure unit

  render state =
    HH.div
      [ HP.classes [ HH.ClassName "Editor" ] ]
      [ HH.div [ HP.classes [ HH.ClassName "Term" ] ] (renderTermWithCursor state.term state.cursor)
      ]

parseKeyboardEvent :: KeyboardEvent -> Array UserAction
parseKeyboardEvent event =
  if cmd && key == "c" then [ UserAction_Core Copy ]
  else if cmd && key == "x" then [ UserAction_Core Copy, UserAction_Core Delete ]
  else if cmd && key == "v" then [ UserAction_Core $ Paste empty ]
  else if key == "Backspace" then [ UserAction_Core Delete ]
  else if key == "(" || key == ")" then [ UserAction_Core $ Paste (pure (Clipboard (Zipper (Span [ Group (Sexp defaultNodeData []) ]) (PointCursor (wrap 0 `consPath` mempty) (wrap 0))))) ]
  else if String.length key == 1 then [ UserAction_Core $ Paste (pure (Clipboard (Zipper (Span [ Atom { label: key } ]) (PointCursor mempty (wrap 1))))) ]
  else Debug.trace (show { key }) \_ -> []
  where
  key = event # KeyboardEvent.key
  shift = event # KeyboardEvent.shiftKey
  cmd = (event # KeyboardEvent.ctrlKey) || (event # KeyboardEvent.metaKey)
