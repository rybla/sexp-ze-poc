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

      -- _ | SpanCursorState mb_mark (SpanCursor pl pr) <- state.cursorState, key == "ArrowLeft" && shift -> do
      --   event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
      --   when (pl <= pr - one) do modify_ _ { cursorState = SpanCursorState mb_mark (state.span # makeSpanCursorFromDrag pl (pr - one)) }

      -- _ | SpanCursorState mb_mark (SpanCursor pl pr) <- state.cursorState, key == "ArrowLeft" -> do
      --   event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
      --   when (wrap 0 <= pl - one) do modify_ _ { cursorState = SpanCursorState mb_mark (state.span # makeSpanCursorFromDrag (pl - one) pr) }

      -- _ | SpanCursorState mb_mark (SpanCursor pl pr) <- state.cursorState, key == "ArrowRight" && shift -> do
      --   event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
      --   when (pl + one <= pr) do modify_ _ { cursorState = SpanCursorState mb_mark (state.span # makeSpanCursorFromDrag (pl + one) pr) }

      -- _ | SpanCursorState mb_mark (SpanCursor pl pr) <- state.cursorState, key == "ArrowRight" -> do
      --   event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
      --   when (pr + one <= wrap (length state.span)) do modify_ _ { cursorState = SpanCursorState mb_mark (state.span # makeSpanCursorFromDrag pl (pr + one)) }

      _ | SpanCursorState Nothing c <- state.cursorState, key == "Escape" -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        let p = endpointLeft c
        modify_ _ { cursorState = SpanCursorState Nothing (SpanCursor p p) }

      _ | SpanCursorState (Just c) _ <- state.cursorState, key == "Escape" -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        modify_ _ { cursorState = SpanCursorState Nothing c }

      _ | ZipperCursorState c <- state.cursorState, key == "Escape" -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        let p = endpointInnerLeft c
        modify_ _ { cursorState = SpanCursorState Nothing (SpanCursor p p) }

      -- expand span cursor

      _ | SpanCursorState _mb_mark (SpanCursor pl pr) <- state.cursorState, shift && key == "ArrowLeft" -> do
        let p1 = pl # shiftPoint (-1)
        when (wrap 0 <= p1) do
          let { unopened, unclosed } = state.span # atSpanCursor (SpanCursor p1 pr) # snd # countUnopenedAndUnclosedParens
          if unopened == 0 && unclosed == 0 then do
            -- a normal span
            modify_ _ { cursorState = SpanCursorState empty (SpanCursor p1 pr) }
          else do
            -- must be a zipper
            case state.span # atIndex (getIndexRightAfterPoint p1) of
              Open -> do
                -- find matching close paren to the right of pr
                let p2 = state.span # getPointRightBeforeNthNextUnopenedParenStartingFromPoint 1 pr
                let p3 = p2 # shiftPoint 1
                modify_ _ { cursorState = ZipperCursorState (ZipperCursor p1 pr p2 p3) }
              Close -> do
                -- find matching open paren to the left of p1
                let p2 = state.span # getPointRightBeforeNthPrevUnclosedParenStartingFromPoint 1 p1
                let p3 = p2 # shiftPoint 1
                modify_ _ { cursorState = ZipperCursorState (ZipperCursor p2 p3 p1 pr) }
              _ -> bug "Editor.component.handleAction" "impossible, since would imply unclosed == unclosed == 0"

      _ | SpanCursorState _mb_mark (SpanCursor pl pr) <- state.cursorState, shift && key == "ArrowRight" -> do
        let p1 = pr # shiftPoint 1
        when (p1 <= wrap (length state.span)) do
          -- just a normal span
          let { unopened, unclosed } = state.span # atSpanCursor (SpanCursor pl p1) # snd # countUnopenedAndUnclosedParens
          if unopened == 0 && unclosed == 0 then do
            -- a normal span 
            modify_ _ { cursorState = SpanCursorState empty (SpanCursor pl p1) }
          else do
            -- must be a zipper
            case state.span # atIndex (getIndexRightBeforePoint p1) of
              Open -> do
                -- find matching close paren to the right of p1
                let p2 = state.span # getPointRightBeforeNthNextUnopenedParenStartingFromPoint 1 p1
                let p3 = p2 # shiftPoint 1
                modify_ _ { cursorState = ZipperCursorState (ZipperCursor pl p1 p2 p3) }
              Close -> do
                -- find matching open paren to the left of pl
                let p2 = state.span # getPointRightBeforeNthPrevUnclosedParenStartingFromPoint 1 pl
                let p3 = p2 # shiftPoint 1
                modify_ _ { cursorState = ZipperCursorState (ZipperCursor p2 p3 pl p1) }
              _ -> bug "Editor.component.handleAction" "impossible, since would imply unclosed == unclosed == 0"

      _ | ZipperCursorState c <- state.cursorState, shift && key == "ArrowLeft" -> do
        pure unit

      _ | ZipperCursorState c <- state.cursorState, shift && key == "ArrowRight" -> do
        pure unit

      -- move point cursor (i.e. empty span cursor) with arrow keys

      _ | key == "ArrowLeft" -> do
        let p = state.cursorState # fromCursorStateToPoint # shiftPoint (-1)
        when (wrap 0 <= p) do
          modify_ _ { cursorState = SpanCursorState empty (SpanCursor p p) }

      _ | key == "ArrowRight" -> do
        let p = state.cursorState # fromCursorStateToPoint # shiftPoint 1
        when (p <= wrap (length state.span)) do
          modify_ _ { cursorState = SpanCursorState empty (SpanCursor p p) }

      -- cut 

      _ | SpanCursorState Nothing c <- state.cursorState, key == "x" && cmd -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        let e = state.span # atSpanCursor c # snd
        let c' /\ span' = deleteAtSpanCursor (c /\ state.span)
        modify_ _ { mb_clipboard = pure (SpanClipboard e), cursorState = SpanCursorState Nothing c', span = span' }

      _ | ZipperCursorState c <- state.cursorState, key == "x" && cmd -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        let z = state.span # atZipperCursor c # snd
        let c' /\ span' = deleteAtZipperCursor (c /\ state.span)
        modify_ _ { mb_clipboard = pure (ZipperClipboard z), cursorState = SpanCursorState empty c', span = span' }

      -- delete

      _ | SpanCursorState Nothing c <- state.cursorState, key == "Backspace" -> do
        let c' /\ span' = deleteAtSpanCursor (c /\ state.span)
        modify_ _ { cursorState = SpanCursorState Nothing c', span = span' }

      _ | ZipperCursorState c <- state.cursorState, key == "Backspace" -> do
        let c' /\ span' = deleteAtZipperCursor (c /\ state.span)
        modify_ _ { cursorState = SpanCursorState empty c', span = span' }

      -- copy

      _ | SpanCursorState mb_mark s <- state.cursorState, key == "c" && cmd -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        let e = state.span # atSpanCursor s # snd
        modify_ _ { mb_clipboard = pure (SpanClipboard e) }

      _ | ZipperCursorState c <- state.cursorState, key == "c" && cmd -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        let z = state.span # atZipperCursor c # snd
        modify_ _ { mb_clipboard = pure (ZipperClipboard z) }

      -- paste

      _ | SpanCursorState Nothing c <- state.cursorState, Just (SpanClipboard e) <- state.mb_clipboard, key == "v" && cmd -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        let c' /\ span' = replaceAtSpanCursor (Zipper e mempty) (c /\ state.span)
        modify_ _ { cursorState = SpanCursorState Nothing c', span = span' }

      _ | SpanCursorState Nothing c <- state.cursorState, Just (ZipperClipboard z) <- state.mb_clipboard, key == "v" && cmd -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        let c' /\ span' = insertAtSpanCursor z (c /\ state.span)
        modify_ _ { cursorState = SpanCursorState Nothing c', span = span' }

      _ | ZipperCursorState c <- state.cursorState, Just (ZipperClipboard z) <- state.mb_clipboard, key == "v" && cmd -> do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
        let c' /\ span' = replaceAtZipperCursor z (c /\ state.span)
        modify_ _ { cursorState = SpanCursorState empty c', span = span' }

      -- insert group

      _ | SpanCursorState Nothing c <- state.cursorState, key == "(" || key == ")" && not cmd -> do
        let c' /\ span' = insertAtSpanCursor (Zipper (Span [ Open ]) (Span [ Close ])) (c /\ state.span)
        modify_ _ { cursorState = SpanCursorState Nothing c', span = span' }

      -- insert atom

      _ | SpanCursorState Nothing c <- state.cursorState, String.length key == 1 && not cmd -> do
        let c' /\ span' = insertAtSpanCursor (Zipper (Span [ Lit key ]) mempty) (c /\ state.span)
        modify_ _ { cursorState = SpanCursorState Nothing c', span = span' }

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
      -- Just p0 -> modify_ _ { cursor = MakeSpanCursor $ makeSpanCursorFromDrag p0 p1 state.span }
      Just p0 /\ SpanCursorState mb_mark _ -> modify_ _ { cursorState = SpanCursorState mb_mark $ makeSpanCursorFromDrag p0 p1 state.span }
      Just p0 /\ ZipperCursorState _ -> modify_ _ { cursorState = SpanCursorState empty $ makeSpanCursorFromDrag p0 p1 state.span }
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
              , HH.span [] [ HH.text "there are two types of cursors: span cursors and zipper cursors" ]
              , HH.span [] $ [ HH.text "a " ] <> [ HH.span [ HP.classes [ HH.ClassName "keyword" ] ] [ HH.text "span cursor" ] ] <> [ HH.text " looks like " ] <> [ HH.span [ HP.classes [ HH.ClassName "code" ] ] [ HH.text "... [ ... ] ..." ] ]
              , HH.span [] $ [ HH.text "a " ] <> [ HH.span [ HP.classes [ HH.ClassName "keyword" ] ] [ HH.text "span cursor" ] ] <> [ HH.text " is on the enclosed expression" ]
              , HH.span [] $ [ HH.text "a " ] <> [ HH.span [ HP.classes [ HH.ClassName "keyword" ] ] [ HH.text "zipper cursor" ] ] <> [ HH.text " looks like " ] <> [ HH.span [ HP.classes [ HH.ClassName "code" ] ] [ HH.text "... { ... [ ... ] ... } ..." ] ]
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
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "( | )" ], HH.td_ [ HH.text "insert group around cursor" ] ]
                      -- movement
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "→" ], HH.td_ [ HH.text "move the right edge of the span to the right" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "⇧→" ], HH.td_ [ HH.text "move the left edge of the span to the right" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "→" ], HH.td_ [ HH.text "move the left edge of the span to the left" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "⇧→" ], HH.td_ [ HH.text "move the right edge of the span to the left" ] ]
                      -- marking
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "Space" ], HH.td_ [ HH.text "if no marker, set marker here" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "Space" ], HH.td_ [ HH.text "if have marker, place zipper cursor between markers" ] ]
                      -- escape
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "Escape" ], HH.td_ [ HH.text "if no marker, shrink cursor to point cursor (empty span cursor)" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "span" ], HH.td_ [ HH.text "Escape" ], HH.td_ [ HH.text "if have marker, set cursor to marker and clear marker" ] ]
                      , HH.tr_ [ HH.td_ [ HH.text "zipper" ], HH.td_ [ HH.text "Escape" ], HH.td_ [ HH.text "set marker to outer side of zipper and cursor to inner side of zipper" ] ]
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
              , HH.div [] [ HH.text "with just keyboard, can't navigate span cursor inside of parentheses (can do this with mouse ofc)" ]
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
      case unit of
        -- on cursor
        _ | p == pl && p == pr ->
          template [ HH.ClassName "Cursor", HH.ClassName "PointCursor" ] "|"
        _ | p == pl ->
          template [ HH.ClassName "Cursor", HH.ClassName "SpanCursor", HH.ClassName "Active" ] "["
        _ | p == pr ->
          template [ HH.ClassName "Cursor", HH.ClassName "SpanCursor", HH.ClassName "Active" ] "]"
        _ | Just (SpanCursor pl' pr') <- mb_mark, p == pl' && p == pr' ->
          template [ HH.ClassName "Cursor", HH.ClassName "PointCursor", HH.ClassName "Marker" ] "|"
        _ | Just (SpanCursor pl' pr') <- mb_mark, p == pl' ->
          template [ HH.ClassName "Cursor", HH.ClassName "SpanCursor", HH.ClassName "Marker" ] "["
        _ | Just (SpanCursor pl' pr') <- mb_mark, p == pr' ->
          template [ HH.ClassName "Cursor", HH.ClassName "SpanCursor", HH.ClassName "Marker" ] "]"
        -- inside cursor
        _ | Nothing <- mb_mark, pl < p && p < pr ->
          template [ HH.ClassName "inside-active-SpanCursor" ] "•"
        _ | Just (SpanCursor pl' pr') <- mb_mark, ((pl <= pl' && pr' <= pr) && ((pl < p && p < pl') || (pr' < p && p < pr))) || ((pl' <= pl && pr <= pr') && (pl < p && p < pr)) ->
          template [ HH.ClassName "inside-active-SpanCursor" ] "•"
        _ | Just (SpanCursor pl' pr') <- mb_mark, ((pl' <= pl && pr <= pr') && ((pl' < p && p < pl) || (pr < p && p < pr'))) || ((pl <= pl' && pr' <= pr) && (pl' < p && p < pr')) ->
          template [ HH.ClassName "inside-marker-SpanCursor" ] "•"
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
        _ | Nothing <- mb_mark, pl `ltPointAndIndex` i && i `ltIndexAndPoint` pr ->
          template [ HH.ClassName "inside-active-SpanCursor" ]
        _
          | Just (SpanCursor pl' pr') <- mb_mark
          , ((pl <= pl' && pr' <= pr) && ((pl `ltPointAndIndex` i && i `ltIndexAndPoint` pl') || (pr' `ltPointAndIndex` i && i `ltIndexAndPoint` pr))) || -- active around marker
              ((pl' <= pl && pr <= pr') && (pl `ltPointAndIndex` i && i `ltIndexAndPoint` pr)) -> -- marker around active 

              template [ HH.ClassName "inside-active-SpanCursor" ]
        _
          | Just (SpanCursor pl' pr') <- mb_mark
          , ((pl' <= pl && pr <= pr') && ((pl' `ltPointAndIndex` i && i `ltIndexAndPoint` pl) || (pr `ltPointAndIndex` i && i `ltIndexAndPoint` pr'))) || -- marker around active
              ((pl <= pl' && pr' <= pr) && (pl' `ltPointAndIndex` i && i `ltIndexAndPoint` pr')) -> -- active around marker

              template [ HH.ClassName "inside-marker-SpanCursor" ]
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
        template [ HH.ClassName "Cursor", HH.ClassName "ZipperCursor" ] "["
      else if p == pir && p == por then
        template [ HH.ClassName "Cursor", HH.ClassName "ZipperCursor" ] "}"
      else if p == pir then
        template [ HH.ClassName "Cursor", HH.ClassName "ZipperCursor" ] "]"
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

