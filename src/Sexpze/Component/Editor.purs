module Sexpze.Component.Editor where

import Prelude

import Control.Monad.State (modify_)
import Control.Plus (empty)
import Data.Array as Array
import Data.List (List(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sexpze.Data.Sexp (Sexp, Sexp'(..))
import Sexpze.Data.Sexp.Cursor (Cursor(..), CursorStatus, Point(..), Span, SubCursorStatus(..), traverseSexpWithCursor)
import Sexpze.Utility (todo)
import Web.Event.Event as Event
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

--------------------------------------------------------------------------------
-- component
--------------------------------------------------------------------------------

data Query a = KeyboardEvent_Query KeyboardEvent a

newtype Input = Input {}

data Output = Updated Term

type Slots :: Row Type
type Slots = ()

type M = Aff

type HTML = H.ComponentHTML Action Slots M

component :: H.Component Query Input Output M
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState _input =
    { term: [ Atom "a", Atom "b" ]
    , cursor: PointCursor (Point Nil 0)
    }

  eval = H.mkEval H.defaultEval { handleAction = handleAction }

  handleAction = case _ of
    UserAction_Action a -> handleUserAction a
    SetCursor_Action c mb_event -> do
      mb_event # maybe (pure unit) (Event.stopPropagation <<< MouseEvent.toEvent) # liftEffect
      modify_ _ { cursor = c }

  render state =
    HH.div
      [ HP.classes [ HH.ClassName "Editor" ] ]
      [ HH.div [ HP.classes [ HH.ClassName "term" ] ]
          [ renderTermNew state.cursor state.term ]
      ]

renderTermNew :: Cursor -> Term -> HTML
renderTermNew cursor =
  traverseSexpWithCursor
    { atom: \label ->
        HH.div [ HP.classes [ H.ClassName "Term", H.ClassName "Atom" ] ]
          [ HH.text label ]
    , group: \xs { last } ->
        HH.div [ HP.classes [ H.ClassName "Term", H.ClassName "Group" ] ]
          ( [ [ renderPunc "(" ]
            , xs
                # map
                    ( \{ before, x: _, r: html_x } ->
                        [ renderCursorHandle before (renderPunc "•")
                        , html_x
                        ]
                    )
                # Array.fold
            , [ renderCursorHandle last (renderPunc "•") ]
            , [ renderPunc ")" ]
            ]
              # Array.fold
          )
    }
    mempty
    (pure (cursor /\ TopSubCursorStatus))
    >>>
      HH.div [ HP.classes [ H.ClassName "Term", H.ClassName "List" ] ]

renderPunc :: String -> HTML
renderPunc s = HH.span [ HP.classes [ HH.ClassName "Punc" ] ] [ HH.text s ]

renderCursorHandle :: Cursor -> HTML -> HTML
renderCursorHandle cursor label =
  HH.div
    [ HE.onClick (SetCursor_Action cursor <<< Just) ]
    [ label ]

renderPointHandle :: Point -> HTML -> HTML
renderPointHandle p label =
  HH.div
    [ HE.onClick (SetCursor_Action (PointCursor p) <<< Just) ]
    [ label ]

renderSpanHandle :: Span -> HTML -> HTML
renderSpanHandle s label =
  HH.div
    [ HE.onClick (SetCursor_Action (SpanCursor s) <<< Just) ]
    [ label ]

fromCursorStatusToClassName :: CursorStatus -> HH.ClassName
fromCursorStatusToClassName = show >>> HH.ClassName

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

type State =
  { term :: Term
  , cursor :: Cursor
  }

type Term = Sexp String
type Term' = Sexp' String

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

--------------------------------------------------------------------------------
-- Action
--------------------------------------------------------------------------------

data Action
  = UserAction_Action UserAction
  | SetCursor_Action Cursor (Maybe MouseEvent)

--------------------------------------------------------------------------------
-- Logic
--------------------------------------------------------------------------------

handleUserAction :: UserAction -> H.HalogenM State Action Slots Output M Unit
handleUserAction MoveLeft = do
  -- { term, cursor } <- get
  -- case moveLeft_Cursor term cursor of
  --   Nothing -> pure unit
  --   Just cursor' -> modify_ _ { cursor = cursor' }
  todo "handleUserAction" {}
handleUserAction MoveRight = do
  -- { term, cursor } <- get
  -- case moveRight_Cursor term cursor of
  --   Nothing -> pure unit
  --   Just cursor' -> modify_ _ { cursor = cursor' }
  todo "handleUserAction" {}
handleUserAction SelectLeft = todo "handleUserAction" {}
handleUserAction SelectRight = todo "handleUserAction" {}
handleUserAction Delete = todo "handleUserAction" {}
handleUserAction Copy = todo "handleUserAction" {}
handleUserAction Paste = todo "handleUserAction" {}
