{-
idea:
- cursor is a span
- by default, an empty span (looks like ibeam)
  - on keyboard, can expand span left or right with a special key
  - on mouse, can expand span by just dragging a new one, or dragging out endpoints of existing span
- selection is between two spans: first navigate cursor (span) to a position, set mark
  - on keyboard, hold shift and make a new cursor (span) by moving and expanding left/right
  - on mouse, hold shift, and make a new cursor (span) by dragging
-}

module Sexpze.Component.Editor where

import Prelude
import Sexpze.Component.State

import Control.Monad.State (get, modify_)
import Control.Plus (empty)
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sexpze.Utility (todo)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type Input =
  { cursor :: Cursor
  , span :: Span
  }

data Query a = KeyboardEvent_Query KeyboardEvent a

data Output = Output

type State =
  { cursor :: Cursor
  , span :: Span
  , mb_clipboard :: Maybe Clipboard
  , mb_dragOrigin :: Maybe Point
  }

data Action
  = Initialize
  | StartDrag Point
  | EndDrag Point

type Slots = ()

type HM = H.HalogenM State Action Slots Output Aff

type HTML = HH.ComponentHTML Action Slots Aff

--------------------------------------------------------------------------------
-- component
--------------------------------------------------------------------------------

component :: H.Component Query Input Output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState { cursor, span } =
    { cursor
    , span
    , mb_clipboard: empty
    , mb_dragOrigin: empty
    }

  eval = H.mkEval H.defaultEval { handleQuery = handleQuery, handleAction = handleAction, initialize = Just Initialize }

  handleQuery :: forall a. Query a -> HM (Maybe a)
  handleQuery (KeyboardEvent_Query event a) = do
    let
      key = event # KeyboardEvent.key
      shift = event # KeyboardEvent.shiftKey
      ctrl = event # KeyboardEvent.ctrlKey
      meta = event # KeyboardEvent.metaKey
      cmd = ctrl || meta
    state <- get

    if false then pure unit
    else if key == "Escape" then do
      modify_ _ { cursor = escapeAtCursor state.cursor }
    else if shift && key == "ArrowLeft" then do
      case state.cursor of
        MakeSpanCursor (SpanCursor pl pr) | 0 < unwrap pl -> do
          let s = state.span # makeSpanCursorFromDrag (pl - one) pr
          modify_ _ { cursor = MakeSpanCursor s }
        _ -> pure unit
    else if key == "ArrowLeft" then do
      case state.cursor of
        MakeSpanCursor (SpanCursor pl pr) | pl == pr && 0 < unwrap pl -> do
          modify_ _ { cursor = MakeSpanCursor $ SpanCursor (pl - one) (pl - one) }
        _ -> pure unit
    else if shift && key == "ArrowRight" then do
      case state.cursor of
        MakeSpanCursor (SpanCursor pl pr) | unwrap pr < length state.span -> do
          let s = state.span # makeSpanCursorFromDrag pl (pr + one)
          modify_ _ { cursor = MakeSpanCursor s }
        _ -> pure unit
    else if key == "ArrowRight" then do
      case state.cursor of
        MakeSpanCursor (SpanCursor pl pr) | pl == pr && unwrap pr < length state.span -> do
          modify_ _ { cursor = MakeSpanCursor $ SpanCursor (pl + one) (pl + one) }
        _ -> pure unit
    else if key == "Backspace" then do
      case state.cursor of
        MakeSpanCursor (SpanCursor pl pr) | pl == pr && wrap 0 < pl -> do
          -- empty span
          let s@(SpanCursor pl' _pr') = makeSpanCursorFromDrag (pl - one) pl state.span
          modify_ _
            { span = state.span # replaceAtSpanCursor (Zipper mempty mempty) s
            , cursor = MakeSpanCursor $ SpanCursor pl' pl'
            }
        c -> do
          let cursor /\ span = deleteAtCursor (c /\ state.span)
          modify_ _ { cursor = cursor, span = span }
    else if not cmd && (key == "(" || key == ")") then do
      let cursor /\ span = insertAtCursor (Zipper (Span [ Open ]) (Span [ Close ])) (state.cursor /\ state.span)
      modify_ _ { cursor = cursor, span = span }
    else if not cmd && String.length key == 1 then do
      let cursor /\ span = insertAtCursor (Zipper (Span [ Lit key ]) mempty) (state.cursor /\ state.span)
      modify_ _ { cursor = cursor, span = span }
    else
      pure unit

    pure (pure a)

  handleAction :: Action -> HM Unit
  handleAction Initialize = do
    pure unit
  handleAction (StartDrag p) = do
    modify_ _ { mb_dragOrigin = pure p }
    pure unit
  handleAction (EndDrag p1) = do
    state <- get
    case state.mb_dragOrigin of
      Nothing -> pure unit
      Just p0 -> modify_ _ { cursor = MakeSpanCursor $ makeSpanCursorFromDrag p0 p1 state.span }
  render state =
    HH.div
      [ HP.classes [ HH.ClassName "Editor" ] ]
      [ HH.div
          [ HP.classes [ HH.ClassName "content" ] ]
          [ renderCursorAndSpan state.cursor state.span ]
      ]

--------------------------------------------------------------------------------
-- render
--------------------------------------------------------------------------------

renderCursorAndSpan :: Cursor -> Span -> HTML
renderCursorAndSpan (MakeSpanCursor c) = renderSpanCursorAndSpan c
renderCursorAndSpan (MakeZipperCursor c) = renderZipperCursorAndSpan c

renderSpanCursorAndSpan :: SpanCursor -> Span -> HTML
renderSpanCursorAndSpan (SpanCursor pl pr) (Span es) =
  HH.div
    [ HP.classes [ HH.ClassName "Span" ] ]
    (es # foldMapPointsAndWithIndex renderPoint renderAtom)
  where

  renderPoint p =
    let
      template cns sym =
        [ HH.div
            [ HP.classes ([ HH.ClassName "Point" ] <> cns)
            , HE.onMouseDown (const (StartDrag p))
            , HE.onMouseUp (const (EndDrag p))
            ]
            [ HH.text sym ]
        ]
    in
      if p == pl && p == pr then
        template [ HH.ClassName "Cursor" ] "|"
      else if p == pl then
        template [ HH.ClassName "Cursor", HH.ClassName "SpanCursor", HH.ClassName "left" ] "["
      else if p == pr then
        template [ HH.ClassName "Cursor", HH.ClassName "SpanCursor", HH.ClassName "right" ] "]"
      else
        template [] "•"

  renderAtom _i a =
    let
      template cns sym =
        [ HH.div
            [ HP.classes ([ HH.ClassName "Atom" ] <> cns) ]
            [ HH.text sym ]
        ]
    in
      case a of
        Lit sym -> template [] sym
        Open -> template [ HH.ClassName "Paren", HH.ClassName "Open" ] "("
        Close -> template [ HH.ClassName "Paren", HH.ClassName "Close" ] ")"

renderZipperCursorAndSpan :: ZipperCursor -> Span -> HTML
renderZipperCursorAndSpan _ _ = todo "renderZipperCursorAndSpan" {}

--------------------------------------------------------------------------------

