module Sexpze.Component.Editor where

import Prelude

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
import Sexpze.Data.Sexp (Sexp, Sexp'(..))
import Sexpze.Data.Sexp.Cursor (Cursor(..), CursorStatus(..), Point(..), Span(..), SubCursorStatus(..), Zipper(..), cursorBetweenPoints, getSubWrapAndSexpAtPath, insertSexpAtPoint, traverseSexpWithCursor)
import Sexpze.Utility (bug, todo)
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
    { term: [ Group [ Atom "a", Atom "b" ] ]
    , cursor: PointCursor (Point (0 : Nil) 0)
    , mb_dragStart: Nothing
    }
  -- { term: [ Group [ Atom "a", Atom "b" ] ]
  -- , cursor: SpanCursor (Span { p0: Point (0 : Nil) 0, p1: Point (0 : Nil) 1 })
  -- , mb_dragStart: Nothing
  -- }
  -- { term: [ Group [ Atom "a", Atom "b", Group [ Atom "c", Atom "d" ], Atom "e", Atom "f" ] ]
  -- , cursor: SpanCursor (Span { p0: Point (0 : Nil) 0, p1: Point (0 : Nil) 1 })
  -- , mb_dragStart: Nothing
  -- }
  -- { term: [ Group [ Atom "a" ] ]
  -- -- , cursor: PointCursor (Point (0 : Nil) 0)
  -- , cursor: ZipperCursor (Zipper (Point Nil 0) (Point (0 : Nil) 0) (Point (0 : Nil) 1) (Point Nil 1))
  -- , mb_dragStart: Nothing
  -- }

  eval = H.mkEval H.defaultEval { handleQuery = handleQuery, handleAction = handleAction }

  handleQuery :: forall a. Query a -> HM (Maybe a)
  handleQuery (KeyboardEvent_Query ke a) = do

    state <- get
    case parseUserActionsFromKeyboardEvent ke state of
      Nothing -> pure unit
      Just acts -> do
        Console.log $ "[Editor.component.handleQuery] " <> show acts
        acts # traverse_ handleUserAction
    pure (pure a)

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

renderMaybeCursorStatus :: Maybe CursorStatus -> HTML
renderMaybeCursorStatus = case _ of
  Nothing -> renderPunc [ H.ClassName "PointHandle", H.ClassName "NoCursorStatus" ] "•"
  Just PointCursorStatus -> renderPunc [ H.ClassName "PointHandle", H.ClassName "PointCursorStatus" ] "|"
  Just SpanStartCursorStatus -> renderPunc [ H.ClassName "PointHandle", H.ClassName "SpanStartCursorStatus" ] "["
  Just SpanEndCursorStatus -> renderPunc [ H.ClassName "PointHandle", H.ClassName "SpanEndCursorStatus" ] "]"
  Just ZipperOuterStartCursorStatus -> renderPunc [ H.ClassName "PointHandle", H.ClassName "ZipperOuterStartCursorStatus" ] "{"
  Just ZipperOuterEndCursorStatus -> renderPunc [ H.ClassName "PointHandle", H.ClassName "ZipperOuterEndCursorStatus" ] "}"
  Just ZipperInnerStartCursorStatus -> renderPunc [ H.ClassName "PointHandle", H.ClassName "ZipperInnerStartCursorStatus" ] "<"
  Just ZipperInnerEndCursorStatus -> renderPunc [ H.ClassName "PointHandle", H.ClassName "ZipperInnerEndCursorStatus" ] ">"

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
                        [ renderPointHandle before.point $ renderMaybeCursorStatus before.status
                        , html_x
                        ]
                    )
                # Array.fold
            , [ renderPointHandle last.point $ renderMaybeCursorStatus last.status
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
  | InsertAtom String
  | InsertGroup

derive instance Generic UserAction _

instance Show UserAction where
  show x = genericShow x

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
  else if cmd && key == "c" then
    pure [ Copy ]
  else if cmd && key == "v" then
    pure [ Paste ]
  else if cmd && key == "x" then
    pure [ Copy, Delete ]
  else if key == "(" then
    pure [ InsertGroup ]
  else if not cmd && String.length key == 1 then do
    pure [ InsertAtom key ]
  else empty
  where
  key = KeyboardEvent.key ke
  shift = KeyboardEvent.shiftKey ke
  cmd = KeyboardEvent.ctrlKey ke || KeyboardEvent.metaKey ke

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
handleUserAction Delete = do
  { cursor, term } <- get
  case cursor of
    PointCursor (Point is j) -> do
      when (j /= 0) do
        let wrap /\ xs = term # getSubWrapAndSexpAtPath is
        modify_ _
          { cursor = PointCursor (Point is (j - 1))
          , term = wrap $ xs # Array.deleteAt (j - 1) # fromMaybe' (\_ -> bug "" "")
          }
        pure unit
    SpanCursor (Span (Point is j0) (Point _ j1)) -> do
      let wrap /\ ys = term # getSubWrapAndSexpAtPath is
      modify_ _
        { cursor = PointCursor (Point is j0)
        , term = wrap $ Array.slice 0 j0 ys <> Array.slice j1 (Array.length ys) ys
        }
    ZipperCursor (Zipper (Point is_outer j0) (Point is_inner j1) (Point _ j2) (Point _ j3)) -> do
      -- outer
      let wrap /\ ys = term # getSubWrapAndSexpAtPath is_outer
      -- inner
      let _ /\ zs = term # getSubWrapAndSexpAtPath is_inner
      modify_ _
        { cursor = PointCursor (Point is_outer j0)
        , term = wrap $ Array.slice 0 j0 ys <> Array.slice j1 j2 zs <> Array.slice j3 (Array.length ys) ys
        }
handleUserAction Copy = todo "handleUserAction" {}
handleUserAction Paste = todo "handleUserAction" {}
handleUserAction (StartDrag start) = do
  { cursor } <- get
  case cursor of
    -- if current cursor is a SpanCursor, and start is one of the endpoints,
    -- then actually set the cursor to the a PointCursor at the other endpoint
    -- of the Span
    SpanCursor (Span p0 p1) | p0 == start -> do
      modify_ _ { mb_dragStart = pure p1 }
    SpanCursor (Span p0 p1) | p1 == start -> do
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
handleUserAction (InsertAtom s) = do
  { cursor, term } <- get
  case cursor of
    PointCursor point -> do
      case insertSexpAtPoint point [ Atom s ] term of
        Just (point' /\ term') -> do
          modify_ _ { cursor = PointCursor point', term = term' }
          pure unit
        Nothing -> pure unit
    _ -> pure unit
handleUserAction InsertGroup = do
  { cursor, term } <- get
  case cursor of
    PointCursor point -> do
      case insertSexpAtPoint point [ Group [] ] term of
        Just (point' /\ term') -> do
          modify_ _ { cursor = PointCursor point', term = term' }
          pure unit
        Nothing -> pure unit
    _ -> pure unit
