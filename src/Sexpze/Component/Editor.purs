module Sexpze.Component.Editor where

import Prelude
import Sexpze.Component.State

import Control.Monad.State (get, modify_)
import Control.Plus (empty)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sexpze.Utility (todo)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type Input =
  { cursor :: Cursor
  , span :: Span
  }

data Query a = KeyboardEvent_Query KeyboardEvent a

data Output = Output

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
  handleQuery (KeyboardEvent_Query _event a) = do
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
renderZipperCursorAndSpan _ _ = todo "" {}

--------------------------------------------------------------------------------

