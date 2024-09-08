module Sexpze.Component.Editor where

import Prelude

import Control.Monad.State (get, modify_)
import Control.Plus (empty)
import Data.Array as Array
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Debug as Debug
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sexpze.Data.Sexp (Sexp, Sexp'(..))
import Sexpze.Data.Sexp.Cursor (Cursor(..), CursorStatus(..), Point(..), Span(..), SubCursorStatus(..), traverseSexpWithCursor)
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
    { term: [ Group [ Atom "a", Atom "b" ] ]
    -- , cursor: PointCursor (Point (0 : Nil) 0)
    , cursor: SpanCursor (Span { p0: Point (0 : Nil) 0, p1: Point (0 : Nil) 1 })
    }

  eval = H.mkEval H.defaultEval { handleAction = handleAction }

  handleAction = case _ of
    UserAction_Action a -> handleUserAction a
    SetCursor_Action cursor mb_event -> do
      mb_event # maybe (pure unit) (Event.stopPropagation <<< MouseEvent.toEvent) # liftEffect
      modify_ _ { cursor = cursor }
    SetSelectOtherPoint_Action p' mb_event -> do
      mb_event # maybe (pure unit) (Event.stopPropagation <<< MouseEvent.toEvent) # liftEffect
      { cursor } <- get
      case cursor of
        PointCursor p -> do
          -- order p and p'
          -- TODO: for now assume the order is p then p'
          whenM (mb_event # maybe true (MouseEvent.button >>> (_ == 0)) # pure) do
            modify_ _ { cursor = SpanCursor (Span { p0: p, p1: p' }) }
        _ -> pure unit

  render state =
    HH.div
      [ HP.classes [ HH.ClassName "Editor" ] ]
      [ HH.div [ HP.classes [ HH.ClassName "term" ] ]
          [ HH.div [ HP.classes [ H.ClassName "Term", H.ClassName "Group" ] ]
              (renderTerm state.cursor state.term)
          ]
      ]

orderPoints :: Point -> Point -> Point /\ Point
orderPoints p0 p1 = p0 /\ p1

renderHandleWithCursorStatus :: Maybe CursorStatus -> HTML
renderHandleWithCursorStatus = case _ of
  Nothing -> renderPunc [ H.ClassName "CursorHandle", H.ClassName "NoCursorStatus" ] "•"
  Just PointCursorStatus -> renderPunc [ H.ClassName "CursorHandle", H.ClassName "PointCursorStatus" ] "|"
  Just SpanBeginCursorStatus -> renderPunc [ H.ClassName "CursorHandle", H.ClassName "SpanBeginCursorStatus" ] "["
  Just SpanEndCursorStatus -> renderPunc [ H.ClassName "CursorHandle", H.ClassName "SpanEndCursorStatus" ] "]"
  Just ZipperOuterBeginCursorStatus -> renderPunc [ H.ClassName "CursorHandle", H.ClassName "ZipperOuterBeginCursorStatus" ] "<{"
  Just ZipperOuterEndCursorStatus -> renderPunc [ H.ClassName "CursorHandle", H.ClassName "ZipperOuterEndCursorStatus" ] "{>"
  Just ZipperInnerBeginCursorStatus -> renderPunc [ H.ClassName "CursorHandle", H.ClassName "ZipperInnerBeginCursorStatus" ] "<}"
  Just ZipperInnerEndCursorStatus -> renderPunc [ H.ClassName "CursorHandle", H.ClassName "ZipperInnerEndCursorStatus" ] "}>"

renderTerm :: Cursor -> Term -> Array HTML
renderTerm cursor =
  traverseSexpWithCursor
    { atom: \label ->
        HH.div [ HP.classes [ H.ClassName "Term", H.ClassName "Atom" ] ]
          [ HH.text label ]
    , group: \xs { last } ->
        HH.div [ HP.classes [ H.ClassName "Term", H.ClassName "Group" ] ]
          ( [ [ renderPunc [] "(" ]
            , xs
                # map
                    ( \{ before, x: _, r: html_x } ->
                        [ renderPointCursorHandle before.point (renderHandleWithCursorStatus before.status)
                        , html_x
                        ]
                    )
                # Array.fold
            , [ renderPointCursorHandle last.point (renderHandleWithCursorStatus last.status)
              ]
            , [ renderPunc [] ")" ]
            ]
              # Array.fold
          )
    }
    mempty
    (pure (cursor /\ PointSubCursorStatus))

renderPunc :: Array H.ClassName -> String -> HTML
renderPunc cns s = HH.span [ HP.classes ([ HH.ClassName "Punc" ] <> cns) ] [ HH.text s ]

renderPointCursorHandle :: Point -> HTML -> HTML
renderPointCursorHandle point label =
  HH.div
    [ HE.onMouseDown (SetCursor_Action (PointCursor point) <<< Just)
    , HE.onMouseUp (SetSelectOtherPoint_Action point <<< Just)
    ]
    [ label
    ]

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
  | SetSelectOtherPoint_Action Point (Maybe MouseEvent)

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
