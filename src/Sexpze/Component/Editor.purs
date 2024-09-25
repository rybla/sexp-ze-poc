{-
idea:
- cursor is a span
- by default, an empty span (looks like ibeam)
  - on keyboard, can shift span left or right with a special key
  - on mouse, can shift span by just dragging a new one, or dragging out endpoints of existing span
- selection is between two spans: first navigate cursor (span) to a position, set mark
  - on keyboard, hold shift and make a new cursor (span) by moving and shifting left/right
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
import Debug as Debug
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
    { cursorState: SpanCursorState (SpanCursor (wrap 0) (wrap 0)) End
    , span
    , mb_clipboard: empty
    , mb_dragOrigin: empty
    }

  eval = H.mkEval H.defaultEval { handleQuery = handleQuery, handleAction = handleAction, initialize = Just Initialize }

  handleQuery :: forall a. Query a -> HM (Maybe a)
  handleQuery (KeyboardEvent_Query event a) = do
    let
      key = event # KeyboardEvent.key
      alt = event # KeyboardEvent.altKey
      shift = event # KeyboardEvent.shiftKey
      ctrl = event # KeyboardEvent.ctrlKey
      meta = event # KeyboardEvent.metaKey
      cmd = ctrl || meta
    state <- get

    case unit of

      -- _ | SpanCursorState Nothing (SpanCursor p _) <- state.cursorState, key == "Escape" -> do
      --   event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
      --   modify_ _ { cursorState = SpanCursorState Nothing (SpanCursor p p) }

      -- _ | SpanCursorState (Just c) _ <- state.cursorState, key == "Escape" -> do
      --   event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
      --   modify_ _ { cursorState = SpanCursorState Nothing c }

      -- _ | ZipperCursorState (ZipperCursor pol pil pir por) <- state.cursorState, key == "Escape" -> do
      --   event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
      --   modify_ _ { cursorState = SpanCursorState (Just (SpanCursor pol por)) (SpanCursor pil pir) }

      -- -- interact with Marker

      -- _ | SpanCursorState Nothing c <- state.cursorState, key == " " -> do
      --   modify_ _ { cursorState = SpanCursorState (pure c) c }

      -- _ | SpanCursorState (Just m) c <- state.cursorState, key == " " -> do
      --   modify_ _ { cursorState = ZipperCursorState (state.span # makeZipperCursorFromSpanCursors m c) }

      -- -- adjust cursor with arrow keys

      -- _ | SpanCursor (SpanCursor pl pr) <- state.cursorState, key == "ArrowLeft" && shift -> do
      --   event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
      --   when (pl <= pr - one) do modify_ _ { cursorState = SpanCursor (state.span # makeSpanCursorFromDrag pl (pr - one)) }

      -- _ | SpanCursor (SpanCursor pl pr) <- state.cursorState, key == "ArrowLeft" -> do
      --   event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
      --   when (wrap 0 <= pl - one) do modify_ _ { cursorState = SpanCursor (state.span # makeSpanCursorFromDrag (pl - one) pr) }

      -- _ | SpanCursor (SpanCursor pl pr) <- state.cursorState, key == "ArrowRight" && shift -> do
      --   event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
      --   when (pl + one <= pr) do modify_ _ { cursorState = SpanCursor (state.span # makeSpanCursorFromDrag (pl + one) pr) }

      -- _ | SpanCursor (SpanCursor pl pr) <- state.cursorState, key == "ArrowRight" -> do
      --   event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
      --   when (pr + one <= wrap (length state.span)) do modify_ _ { cursorState = SpanCursor (state.span # makeSpanCursorFromDrag pl (pr + one)) }

      _ | SpanCursorState c o <- state.cursorState, key == "Escape" -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        let p = endpointOfSpanCursor o c
        modify_ _ { cursorState = SpanCursorState (SpanCursor p p) o }

      _ | ZipperCursorState c o <- state.cursorState, key == "Escape" -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        let c' /\ o' = fromZipperCursorWithOrientationToSpanCursorWithOrientation (c /\ o)
        modify_ _ { cursorState = SpanCursorState c' o' }

      -- convert span to zipper; shift zipper cursor

      _ | SpanCursorState c o <- state.cursorState, shift && alt && key == "ArrowLeft" -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        let
          o' = case o of
            Start -> Outer /\ Start
            End -> Inner /\ End
        case state.span # shiftBackwardZipperCursorWithOrientation (fromSpanCursorToEmptyZipperCursor c) o' of
          Nothing -> pure unit
          Just cs -> modify_ _ { cursorState = cs }

      _ | SpanCursorState c o <- state.cursorState, shift && alt && key == "ArrowRight" -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        let
          o' = case o of
            Start -> Inner /\ Start
            End -> Outer /\ End
        case state.span # shiftForwardZipperCursorWithOrientation (fromSpanCursorToEmptyZipperCursor c) o' of
          Nothing -> pure unit
          Just cs -> modify_ _ { cursorState = cs }

      -- shift span cursor

      _ | SpanCursorState c o <- state.cursorState, shift && key == "ArrowLeft" ->
        case state.span # shiftBackwardSpanCursorWithOrientation c o of
          Nothing -> pure unit
          Just cs -> modify_ _ { cursorState = cs }

      _ | SpanCursorState c o <- state.cursorState, shift && key == "ArrowRight" -> do
        case shiftForwardSpanCursorWithOrientation c o state.span of
          Nothing -> pure unit
          Just cs -> modify_ _ { cursorState = cs }

      -- shift zipper cursor

      _ | ZipperCursorState c o <- state.cursorState, shift && key == "ArrowLeft" -> do
        case state.span # shiftBackwardZipperCursorWithOrientation c o of
          Nothing -> pure unit
          Just cs -> modify_ _ { cursorState = cs }

      _ | ZipperCursorState c o <- state.cursorState, shift && key == "ArrowRight" -> do
        case state.span # shiftForwardZipperCursorWithOrientation c o of
          Nothing -> pure unit
          Just cs -> modify_ _ { cursorState = cs }

      -- shift point cursor

      _ | key == "ArrowLeft" -> do
        let p = state.cursorState # fromCursorStateToPoint
        modify_ _ { cursorState = SpanCursorState (SpanCursor p p) End }
        let p' = p # shiftPoint (-1)
        when (wrap 0 <= p') do
          modify_ _ { cursorState = SpanCursorState (SpanCursor p' p') End }

      _ | key == "ArrowRight" -> do
        let p = state.cursorState # fromCursorStateToPoint
        modify_ _ { cursorState = SpanCursorState (SpanCursor p p) End }
        let p' = p # shiftPoint 1
        when (p' <= wrap (length state.span)) do
          modify_ _ { cursorState = SpanCursorState (SpanCursor p' p') End }

      -- cut 

      _ | SpanCursorState c o <- state.cursorState, key == "x" && cmd -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        let e = state.span # atSpanCursor c # snd
        let c' /\ span' = deleteAtSpanCursor (c /\ state.span)
        modify_ _
          { mb_clipboard = pure (SpanClipboard e)
          , cursorState = SpanCursorState c' End
          , span = span'
          }

      _ | ZipperCursorState c o <- state.cursorState, key == "x" && cmd -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        let z = state.span # atZipperCursor c # snd
        let c' /\ span' = deleteAtZipperCursor (c /\ state.span)
        modify_ _
          { mb_clipboard = pure (ZipperClipboard z)
          , cursorState = SpanCursorState c' End
          , span = span'
          }

      -- delete

      _ | SpanCursorState c o <- state.cursorState, key == "Backspace" -> do
        let c' /\ span' = deleteAtSpanCursor (c /\ state.span)
        modify_ _ { cursorState = SpanCursorState c' o, span = span' }

      _ | ZipperCursorState c o <- state.cursorState, key == "Backspace" -> do
        let c' /\ span' = deleteAtZipperCursor (c /\ state.span)
        modify_ _ { cursorState = SpanCursorState c' End, span = span' }

      -- copy

      _ | SpanCursorState s o <- state.cursorState, key == "c" && cmd -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        let e = state.span # atSpanCursor s # snd
        modify_ _ { mb_clipboard = pure (SpanClipboard e) }

      _ | ZipperCursorState c o <- state.cursorState, key == "c" && cmd -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        let z = state.span # atZipperCursor c # snd
        modify_ _ { mb_clipboard = pure (ZipperClipboard z) }

      -- paste

      _ | SpanCursorState c o <- state.cursorState, Just (SpanClipboard e) <- state.mb_clipboard, key == "v" && cmd -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        let c' /\ span' = replaceAtSpanCursor (Zipper e mempty) (c /\ state.span)
        modify_ _ { cursorState = SpanCursorState c' o, span = span' }

      _ | SpanCursorState c o <- state.cursorState, Just (ZipperClipboard z) <- state.mb_clipboard, key == "v" && cmd -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        let c' /\ span' = insertAtSpanCursor z (c /\ state.span)
        modify_ _ { cursorState = SpanCursorState c' o, span = span' }

      _ | ZipperCursorState c o <- state.cursorState, Just (ZipperClipboard z) <- state.mb_clipboard, key == "v" && cmd -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        let c' /\ span' = replaceAtZipperCursor z (c /\ state.span)
        modify_ _ { cursorState = SpanCursorState c' (fromZipperCursorOrientationToSpanCursorOrientation o), span = span' }

      -- insert group

      _ | SpanCursorState c o <- state.cursorState, key == "(" || key == ")" && not cmd -> do
        let c' /\ span' = insertAtSpanCursor (Zipper (Span [ Open ]) (Span [ Close ])) (c /\ state.span)
        modify_ _ { cursorState = SpanCursorState c' o, span = span' }

      -- insert atom

      _ | SpanCursorState c o <- state.cursorState, String.length key == 1 && not cmd -> do
        let c' /\ span' = insertAtSpanCursor (Zipper (Span [ Lit key ]) mempty) (c /\ state.span)
        modify_ _ { cursorState = SpanCursorState c' o, span = span' }

      _ -> do
        Console.log $ "unrecognized keyboard event: " <> show { key, shift, ctrl, meta }
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
    case state.mb_dragOrigin /\ state.cursorState of
      Nothing /\ _ -> pure unit
      Just p0 /\ _ -> modify_ _ { cursorState = SpanCursorState (makeSpanCursorFromDrag p0 p1 state.span) (if p0 < p1 then End else Start) }
  render state = Debug.trace (show state) \_ ->
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
              , HH.span [] $ [ HH.text "there are two types of cursors: " ] <> [ HH.span [ HP.classes [ HH.ClassName "keyword" ] ] [ HH.text "span cursors" ] ] <> [ HH.text " and " ] <> [ HH.span [ HP.classes [ HH.ClassName "keyword" ] ] [ HH.text "zipper cursors" ] ]
              , HH.span [] $ [ HH.text "a " ] <> [ HH.span [ HP.classes [ HH.ClassName "keyword" ] ] [ HH.text "span cursor" ] ] <> [ HH.text " looks like " ] <> [ HH.span [ HP.classes [ HH.ClassName "code" ] ] [ HH.text "... { ... } ..." ] ]
              , HH.span [] $ [ HH.text "a " ] <> [ HH.span [ HP.classes [ HH.ClassName "keyword" ] ] [ HH.text "span cursor" ] ] <> [ HH.text " is on the enclosed expression" ]
              , HH.span [] $ [ HH.text "a " ] <> [ HH.span [ HP.classes [ HH.ClassName "keyword" ] ] [ HH.text "zipper cursor" ] ] <> [ HH.text " looks like " ] <> [ HH.span [ HP.classes [ HH.ClassName "code" ] ] [ HH.text "... { ... { ... } ... } ..." ] ]
              , HH.span [] $ [ HH.text "a " ] <> [ HH.span [ HP.classes [ HH.ClassName "keyword" ] ] [ HH.text "zipper cursor" ] ] <> [ HH.text " is on the enclosed one-hole context" ]
              ]
          , HH.div
              [ HP.classes [ HH.ClassName "block" ] ]
              [ HH.div [ HP.classes [ HH.ClassName "title" ] ] [ HH.text "controls" ]
              , HH.table_
                  [ HH.thead_
                      [ HH.tr_ [ HH.th_ [ HH.text "cursor" ], HH.th_ [ HH.text "input" ], HH.th_ [ HH.text "effect" ] ] ]
                  , HH.tbody_
                      [ -- insertion
                        HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "<char>" ], HH.td_ [ HH.text "insert atom right before cursor" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "(" ], HH.td_ [ HH.text "insert group around cursor" ] ]
                      -- movement
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "→" ], HH.td_ [ HH.text "move point to right" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "←" ], HH.td_ [ HH.text "move point to left" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "span | zipper" ], HH.td_ [ HH.text "⇧→" ], HH.td_ [ HH.text "shift active edge to right" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "span | zipper" ], HH.td_ [ HH.text "⇧←" ], HH.td_ [ HH.text "shift active edge to left" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "zipper" ], HH.td_ [ HH.text "⎇⇧→" ], HH.td_ [ HH.text "begin zipper from cursor and shift to right" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "zipper" ], HH.td_ [ HH.text "⎇⇧←" ], HH.td_ [ HH.text "begin zipper from cursor and to left" ] ]
                      -- escape
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "Escape" ], HH.td_ [ HH.text "shrink cursor to point" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "zipper" ], HH.td_ [ HH.text "Escape" ], HH.td_ [ HH.text "jump back to starting span cursor" ] ]
                      -- misc
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "⌫" ], HH.td_ [ HH.text "delete the expression under the cursor" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "zipper" ], HH.td_ [ HH.text "⌫" ], HH.td_ [ HH.text "delete the zipper under the cursor" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "^c" ], HH.td_ [ HH.text "copy the expression under the cursor" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "zipper" ], HH.td_ [ HH.text "^c" ], HH.td_ [ HH.text "copy the zipper under the cursor" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "^x" ], HH.td_ [ HH.text "cut the expression under the cursor" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "zipper" ], HH.td_ [ HH.text "^x" ], HH.td_ [ HH.text "cut the zipper under the cursor" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "^v" ], HH.td_ [ HH.text "paste a span clipboard in place of the cursor" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "^v" ], HH.td_ [ HH.text "paste a zipper clipboard around the cursor" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "zipper" ], HH.td_ [ HH.text "^v" ], HH.td_ [ HH.text "paste a zipper clipboard in place of the cursor" ] ]
                      ]
                  ]
              ]
          , HH.div
              [ HP.classes [ HH.ClassName "block" ] ]
              [ HH.div [ HP.classes [ HH.ClassName "title" ] ] [ HH.text "problems" ]
              , HH.div [] [ HH.text """can't just "shift-then-move-over" with keyboard, since what are the intermediary states to that? would have to sort of "push" the RHS of inner zipper to the right in order to do this, but instead i have them swap positions. perhaps "pushing" like this is what should happen instead?""" ]
              ]
          ]
      ]

--------------------------------------------------------------------------------
-- render
--------------------------------------------------------------------------------

renderCursorStateAndSpan :: CursorState -> Span -> HTML
renderCursorStateAndSpan (SpanCursorState c o) = renderSpanCursorStateAndSpan c
renderCursorStateAndSpan (ZipperCursorState c o) = renderZipperCursorStateAndSpan c

renderSpanCursorStateAndSpan :: SpanCursor -> Span -> HTML
renderSpanCursorStateAndSpan (SpanCursor pl pr) (Span es) =
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
      case unit of
        -- on cursor
        _ | p == pl && p == pr ->
          template [ HH.ClassName "Cursor", HH.ClassName "PointCursor" ] "|"
        _ | p == pl ->
          template [ HH.ClassName "Cursor", HH.ClassName "SpanCursor", HH.ClassName "Active" ] "{"
        _ | p == pr ->
          template [ HH.ClassName "Cursor", HH.ClassName "SpanCursor", HH.ClassName "Active" ] "}"
        -- inside cursor
        _ | pl < p && p < pr ->
          template [ HH.ClassName "inside-active-SpanCursor" ] "•"
        _ ->
          template [] "•"

  renderAtom i a =
    let
      template cns =
        [ HH.div
            [ HP.classes
                ( [ HH.ClassName "Atom" ]
                    <>
                      ( case a of
                          Lit sym -> []
                          Open -> [ HH.ClassName "Paren", HH.ClassName "Open" ]
                          Close -> [ HH.ClassName "Paren", HH.ClassName "Close" ]
                      )
                    <> cns
                )
            ]
            [ HH.text case a of
                Lit sym -> sym
                Open -> "("
                Close -> ")"
            ]
        ]
    in
      case unit of
        _ | pl `ltPointAndIndex` i && i `ltIndexAndPoint` pr ->
          template [ HH.ClassName "inside-active-SpanCursor" ]
        _ -> template []

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
        template [ HH.ClassName "Cursor" ] "|"
      else if p == pil && p == pir then
        template [ HH.ClassName "Cursor" ] "|"
      else if p == pol && p == pil then
        template [ HH.ClassName "Cursor", HH.ClassName "ZipperCursor" ] "{"
      else if p == pol then
        template [ HH.ClassName "Cursor", HH.ClassName "ZipperCursor" ] "{"
      else if p == pil then
        template [ HH.ClassName "Cursor", HH.ClassName "ZipperCursor" ] "{"
      else if p == pir && p == por then
        template [ HH.ClassName "Cursor", HH.ClassName "ZipperCursor" ] "}"
      else if p == pir then
        template [ HH.ClassName "Cursor", HH.ClassName "ZipperCursor" ] "}"
      else if p == por then
        template [ HH.ClassName "Cursor", HH.ClassName "ZipperCursor" ] "}"
      else if pol <= p && p <= pil then
        template [ HH.ClassName "inside-ZipperCursor" ] "•"
      else if pir <= p && p <= por then
        template [ HH.ClassName "inside-ZipperCursor" ] "•"
      else
        template [] "•"

  renderAtom i a =
    let
      template cns =
        [ HH.div
            [ HP.classes
                ( [ HH.ClassName "Atom" ]
                    <>
                      ( case a of
                          Lit sym -> [ HH.ClassName $ "Lit-" <> sym ]
                          Open -> [ HH.ClassName "Paren", HH.ClassName "Open" ]
                          Close -> [ HH.ClassName "Paren", HH.ClassName "Close" ]
                      )
                    <> cns
                )
            ]
            [ HH.text case a of
                Lit sym -> sym
                Open -> "("
                Close -> ")"
            ]
        ]
    in
      if pol `ltPointAndIndex` i && i `ltIndexAndPoint` pil then
        template [ HH.ClassName "inside-ZipperCursor" ]
      else if pir `ltPointAndIndex` i && i `ltIndexAndPoint` por then
        template [ HH.ClassName "inside-ZipperCursor" ]
      else
        template []

--------------------------------------------------------------------------------

