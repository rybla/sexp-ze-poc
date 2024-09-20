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
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sexpze.Utility (todo)
import Web.Event.Event as Event
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type Input =
  { span :: Span
  }

data Query a = KeyboardEvent_Query KeyboardEvent a

data Output = Output

type State =
  { cursorState :: CursorState
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
  initialState { span } =
    { cursorState: SpanCursorState empty (SpanCursor (wrap 0) (wrap 0))
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

    case unit of
      -- _ | key == "Escape" -> modify_ _ { cursor = escapeAtCursor state.cursor }

      _ | SpanCursorState Nothing c <- state.cursorState, key == " " -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        modify_ _ { cursorState = SpanCursorState (pure c) c }

      _ | SpanCursorState (Just m) c <- state.cursorState, key == " " -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        modify_ _ { cursorState = ZipperCursorState (state.span # makeZipperCursorFromSpanCursors m c) }

      _ | SpanCursorState mb_mark (SpanCursor pl pr) <- state.cursorState, key == "ArrowLeft" && shift -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        when (pl <= pr - one) do modify_ _ { cursorState = SpanCursorState mb_mark (state.span # makeSpanCursorFromDrag pl (pr - one)) }

      _ | SpanCursorState mb_mark (SpanCursor pl pr) <- state.cursorState, key == "ArrowLeft" -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        when (wrap 0 <= pl - one) do modify_ _ { cursorState = SpanCursorState mb_mark (state.span # makeSpanCursorFromDrag (pl - one) pr) }

      _ | SpanCursorState mb_mark (SpanCursor pl pr) <- state.cursorState, key == "ArrowRight" && shift -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        when (pl + one <= pr) do modify_ _ { cursorState = SpanCursorState mb_mark (state.span # makeSpanCursorFromDrag (pl + one) pr) }

      _ | SpanCursorState mb_mark (SpanCursor pl pr) <- state.cursorState, key == "ArrowRight" -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        when (pr + one <= wrap (length state.span)) do modify_ _ { cursorState = SpanCursorState mb_mark (state.span # makeSpanCursorFromDrag pl (pr + one)) }

      _ | SpanCursorState mb_mark c <- state.cursorState, key == "x" && cmd -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        let e' = state.span # atSpanCursor c # snd
        let c' /\ span' = deleteAtSpanCursor (c /\ state.span)
        modify_ _ { mb_clipboard = pure (SpanClipboard e'), cursorState = SpanCursorState mb_mark c', span = span' }

      _ | SpanCursorState mb_mark c <- state.cursorState, key == "Backspace" -> do
        let c' /\ span' = deleteAtSpanCursor (c /\ state.span)
        modify_ _ { cursorState = SpanCursorState mb_mark c', span = span' }

      _ | SpanCursorState mb_mark s <- state.cursorState, key == "c" && cmd -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        let e' = state.span # atSpanCursor s # snd
        modify_ _ { mb_clipboard = pure (SpanClipboard e') }

      _ | SpanCursorState mb_mark c <- state.cursorState, Just (SpanClipboard e') <- state.mb_clipboard, key == "v" && cmd -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        let c' /\ span' = replaceAtSpanCursor (Zipper e' mempty) (c /\ state.span)
        modify_ _ { mb_clipboard = pure (SpanClipboard e'), cursorState = SpanCursorState mb_mark c', span = span' }

      _ | SpanCursorState mb_mark c <- state.cursorState, key == "(" || key == ")" && not cmd -> do
        let c' /\ span' = insertAtSpanCursor (Zipper (Span [ Open ]) (Span [ Close ])) (c /\ state.span)
        modify_ _ { cursorState = SpanCursorState mb_mark c', span = span' }

      _ | SpanCursorState mb_mark c <- state.cursorState, String.length key == 1 && not cmd -> do
        let c' /\ span' = insertAtSpanCursor (Zipper (Span [ Lit key ]) mempty) (c /\ state.span)
        modify_ _ { cursorState = SpanCursorState mb_mark c', span = span' }

      _ -> pure unit

    pure (pure a)

  handleAction :: Action -> HM Unit
  handleAction Initialize = do
    pure unit
  handleAction (StartDrag p) = do
    modify_ _ { mb_dragOrigin = pure p }
    pure unit
  handleAction (EndDrag p1) = do
    state <- get
    case state.mb_dragOrigin /\ state.cursorState of
      Nothing /\ _ -> pure unit
      -- Just p0 -> modify_ _ { cursor = MakeSpanCursor $ makeSpanCursorFromDrag p0 p1 state.span }
      Just p0 /\ SpanCursorState mb_mark _ -> modify_ _ { cursorState = SpanCursorState mb_mark $ makeSpanCursorFromDrag p0 p1 state.span }
      Just p0 /\ ZipperCursorState _ -> modify_ _ { cursorState = SpanCursorState empty $ makeSpanCursorFromDrag p0 p1 state.span }
  render state =
    HH.div
      [ HP.classes [ HH.ClassName "Editor" ] ]
      [ HH.div
          [ HP.classes [ HH.ClassName "content" ] ]
          [ renderCursorStateAndSpan state.cursorState state.span ]
      , HH.div
          [ HP.classes [ HH.ClassName "information" ] ]
          [ HH.div
              [ HP.classes [ HH.ClassName "block" ] ]
              [ HH.div [ HP.classes [ HH.ClassName "title" ] ] [ HH.text "concepts" ]
              , HH.div [] [ HH.text "there are two types of cursors, spans and zippers" ]
              , HH.div [] [ HH.text "a span cursor looks like `... [ ... ] ...`; a span cursor is on the enclosed expression" ]
              , HH.div [] [ HH.text "a zipper cursor looks like `... { ... [ ... ] ... } ...`; a zipper cursor is on the enclosed one-hole context" ]
              ]
          , HH.div
              [ HP.classes [ HH.ClassName "block" ] ]
              [ HH.div [ HP.classes [ HH.ClassName "title" ] ] [ HH.text "controls" ]
              , HH.table_
                  [ HH.thead_
                      [ HH.tr_ [ HH.th_ [ HH.text "cursor" ], HH.th_ [ HH.text "input" ], HH.th_ [ HH.text "effect" ] ] ]
                  , HH.tbody_
                      [ HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "→" ], HH.td_ [ HH.text "move the right edge of the span to the right" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "⇧→" ], HH.td_ [ HH.text "move the left edge of the span to the right" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "→" ], HH.td_ [ HH.text "move the left edge of the span to the left" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "⇧→" ], HH.td_ [ HH.text "move the right edge of the span to the left" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "⌘c" ], HH.td_ [ HH.text "copy the expression under the cursor" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "⌘x" ], HH.td_ [ HH.text "cut the expression under the cursor" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "⌘v" ], HH.td_ [ HH.text "paste a span clipboard in place of the cursor" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "⌘v" ], HH.td_ [ HH.text "paste a zipper clipboard around the cursor" ] ]
                      ]
                  ]
              ]
          ]
      ]

--------------------------------------------------------------------------------
-- render
--------------------------------------------------------------------------------

renderCursorStateAndSpan :: CursorState -> Span -> HTML
renderCursorStateAndSpan (SpanCursorState mb_mark c) = renderSpanCursorStateAndSpan mb_mark c
renderCursorStateAndSpan (ZipperCursorState c) = renderZipperCursorStateAndSpan c

renderSpanCursorStateAndSpan :: Maybe SpanCursor -> SpanCursor -> Span -> HTML
renderSpanCursorStateAndSpan mb_mark (SpanCursor pl pr) (Span es) =
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
        template [ HH.ClassName "Cursor", HH.ClassName "SpanCursor", HH.ClassName "left", HH.ClassName "Active" ] "["
      else if p == pr then
        template [ HH.ClassName "Cursor", HH.ClassName "SpanCursor", HH.ClassName "right", HH.ClassName "Active" ] "]"
      else if pure p == (mb_mark <#> endpointLeft) && pure p == (mb_mark <#> endpointRight) then
        template [ HH.ClassName "Cursor", HH.ClassName "Marker" ] "|"
      else if pure p == (mb_mark <#> endpointLeft) then
        template [ HH.ClassName "Cursor", HH.ClassName "SpanCursor", HH.ClassName "left", HH.ClassName "Marker" ] "["
      else if pure p == (mb_mark <#> endpointRight) then
        template [ HH.ClassName "Cursor", HH.ClassName "SpanCursor", HH.ClassName "right", HH.ClassName "Marker" ] "]"
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

renderZipperCursorStateAndSpan :: ZipperCursor -> Span -> HTML
renderZipperCursorStateAndSpan (ZipperCursor pol pil pir por) (Span es) =
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
      if p == pol && p == pil && p == pir && p == por then
        template [ HH.ClassName "Cursor" ] "||"
      else if p == pol && p == pil then
        template [ HH.ClassName "Cursor", HH.ClassName "ZipperCursor", HH.ClassName "outer-left-and-inner-left" ] "{"
      else if p == pol then
        template [ HH.ClassName "Cursor", HH.ClassName "ZipperCursor", HH.ClassName "outer-left" ] "{"
      else if p == pil then
        template [ HH.ClassName "Cursor", HH.ClassName "ZipperCursor", HH.ClassName "inner-left" ] "["
      else if p == pir && p == por then
        template [ HH.ClassName "Cursor", HH.ClassName "ZipperCursor", HH.ClassName "inner-right-and-outer-right" ] "}"
      else if p == pir then
        template [ HH.ClassName "Cursor", HH.ClassName "ZipperCursor", HH.ClassName "inner-right" ] "]"
      else if p == por then
        template [ HH.ClassName "Cursor", HH.ClassName "ZipperCursor", HH.ClassName "outer-right" ] "}"
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

--------------------------------------------------------------------------------

