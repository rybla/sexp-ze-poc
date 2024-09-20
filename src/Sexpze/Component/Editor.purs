module Sexpze.Component.Editor where

import Prelude
import Sexpze.Component.State

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
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

data Action = Initialize

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
    }

  eval = H.mkEval H.defaultEval { handleQuery = handleQuery, handleAction = handleAction, initialize = Just Initialize }

  handleQuery :: forall a. Query a -> HM (Maybe a)
  handleQuery (KeyboardEvent_Query _event a) = do
    pure (pure a)

  handleAction :: Action -> HM Unit
  handleAction Initialize = do
    pure unit

  render state = HH.div [] [ HH.text "<Editor/>" ]

--------------------------------------------------------------------------------
-- render
--------------------------------------------------------------------------------

renderCursorAndSpan :: Cursor -> Span -> HTML
renderCursorAndSpan (MakeSpanCursor c) = renderSpanCursorAndSpan c
renderCursorAndSpan (MakeZipperCursor c) = renderZipperCursorAndSpan c

renderSpanCursorAndSpan :: SpanCursor -> Span -> HTML
renderSpanCursorAndSpan = todo "" {}

renderZipperCursorAndSpan :: ZipperCursor -> Span -> HTML
renderZipperCursorAndSpan = todo "" {}

