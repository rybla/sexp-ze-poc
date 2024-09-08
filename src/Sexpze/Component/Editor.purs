module Sexpze.Component.Editor where

import Prelude

import Control.Monad.State (get, modify_)
import Control.Plus (empty)
import Data.Array as Array
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sexpze.Data.Sexp (Sexp, Sexp'(..))
import Sexpze.Data.Sexp.Cursor (Cursor(..), CursorStatus(..), Point(..), Span(..), SubCursorStatus(..), cursorBetweenPoints, traverseSexpWithCursor)
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
type HM = H.HalogenM State Action Slots Output M

type HTML = H.ComponentHTML Action Slots M

component :: H.Component Query Input Output M
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState _input =
    -- { term: [ Group [ Atom "a", Atom "b" ] ]
    -- , cursor: PointCursor (Point (0 : Nil) 0)
    -- }
    -- { term: [ Group [ Atom "a", Atom "b" ] ]
    -- , cursor: SpanCursor (Span { p0: Point (0 : Nil) 0, p1: Point (0 : Nil) 1 })
    -- }
    { term: [ Group [ Atom "a", Atom "b", Group [ Atom "c", Atom "d" ], Atom "e", Atom "f" ] ]
    , cursor: SpanCursor (Span { p0: Point (0 : Nil) 0, p1: Point (0 : Nil) 1 })
    , mb_dragStart: Nothing
    }

  eval = H.mkEval H.defaultEval { handleAction = handleAction }

  handleAction = case _ of
    UserAction_Action a config -> do
      handleActionConfig config
      handleUserAction a

  render state =
    HH.div
      [ HP.classes [ HH.ClassName "Editor" ] ]
      [ HH.div [ HP.classes [ HH.ClassName "term" ] ]
          [ HH.div [ HP.classes [ H.ClassName "Term", H.ClassName "Group" ] ]
              [ renderTerm state.cursor state.term ]
          ]
      ]

renderHandleWithCursorStatus :: Maybe CursorStatus -> HTML
renderHandleWithCursorStatus = case _ of
  Nothing -> renderPunc [ H.ClassName "CursorHandle", H.ClassName "NoCursorStatus" ] "•"
  Just PointCursorStatus -> renderPunc [ H.ClassName "CursorHandle", H.ClassName "PointCursorStatus" ] "|"
  Just SpanStartCursorStatus -> renderPunc [ H.ClassName "CursorHandle", H.ClassName "SpanStartCursorStatus" ] "["
  Just SpanEndCursorStatus -> renderPunc [ H.ClassName "CursorHandle", H.ClassName "SpanEndCursorStatus" ] "]"
  Just ZipperOuterStartCursorStatus -> renderPunc [ H.ClassName "CursorHandle", H.ClassName "ZipperOuterStartCursorStatus" ] "<{"
  Just ZipperOuterEndCursorStatus -> renderPunc [ H.ClassName "CursorHandle", H.ClassName "ZipperOuterEndCursorStatus" ] "{>"
  Just ZipperInnerStartCursorStatus -> renderPunc [ H.ClassName "CursorHandle", H.ClassName "ZipperInnerStartCursorStatus" ] "<}"
  Just ZipperInnerEndCursorStatus -> renderPunc [ H.ClassName "CursorHandle", H.ClassName "ZipperInnerEndCursorStatus" ] "}>"

renderTerm :: Cursor -> Term -> HTML
renderTerm cursor =
  traverseSexpWithCursor
    { atom: \label ->
        HH.div [ HP.classes [ H.ClassName "Term", H.ClassName "Atom" ] ]
          [ HH.text label ]
    , group: \{ first, last } html ->
        HH.div [ HP.classes [ H.ClassName "Term", H.ClassName "Group" ] ]
          [ renderPointHandle first.point $ renderPunc [] "("
          , html
          , renderPointHandle last.point $ renderPunc [] ")"
          ]
    , list: \xs { last } ->
        HH.div [ HP.classes [ H.ClassName "Term", H.ClassName "List" ] ]
          ( [ xs
                # map
                    ( \{ before, x: _, r: html_x } ->
                        [ renderPointHandle before.point $ renderHandleWithCursorStatus before.status
                        , html_x
                        ]
                    )
                # Array.fold
            , [ renderPointHandle last.point $ renderHandleWithCursorStatus last.status
              ]
            ]
              # Array.fold
          )
    }
    mempty
    (pure (cursor /\ PointSubCursorStatus))

renderPunc :: Array H.ClassName -> String -> HTML
renderPunc cns s = HH.span [ HP.classes ([ HH.ClassName "Punc" ] <> cns) ] [ HH.text s ]

renderPointHandle :: Point -> HTML -> HTML
renderPointHandle point label =
  HH.div
    [ HE.onMouseDown
        ( \event ->
            UserAction_Action
              (StartDrag point)
              (MouseActionConfig { event, doStopPropagation: true })
        )
    , HE.onMouseUp
        ( \event ->
            UserAction_Action
              (EndDrag point)
              (MouseActionConfig { event, doStopPropagation: true })
        )
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
  , mb_dragStart :: Maybe Point
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
  | StartDrag Point
  | EndDrag Point

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

data Action = UserAction_Action UserAction ActionConfig

data ActionConfig = MouseActionConfig
  { event :: MouseEvent
  , doStopPropagation :: Boolean
  }

handleActionConfig :: ActionConfig -> HM Unit
handleActionConfig (MouseActionConfig args) = do
  when args.doStopPropagation do
    args.event # MouseEvent.toEvent # Event.stopPropagation # liftEffect
  pure unit

handleUserAction :: UserAction -> HM Unit
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
handleUserAction (StartDrag start) = do
  { cursor } <- get
  case cursor of
    -- if current cursor is a SpanCursor, and start is one of the endpoints,
    -- then actually set the cursor to the a PointCursor at the other endpoint
    -- of the Span
    SpanCursor (Span { p0, p1 }) | p0 == start -> do
      modify_ _ { mb_dragStart = pure p1 }
    SpanCursor (Span { p0, p1 }) | p1 == start -> do
      modify_ _ { mb_dragStart = pure p0 }
    _ -> do
      modify_ _ { mb_dragStart = pure start }
handleUserAction (EndDrag end) = do
  { term, mb_dragStart } <- get
  case mb_dragStart of
    Nothing -> do
      -- TODO: this shouldn't happen
      pure unit
    Just start -> do
      pure unit
      modify_ _
        { cursor = cursorBetweenPoints term start end
        , mb_dragStart = Nothing
        }

