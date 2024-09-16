module Sexpze.Component.Editor where

import Prelude
import Sexpze.Data.Sexp.Cursor

import Control.Monad.State (get, modify_)
import Control.Plus (empty)
import DOM.HTML.Indexed as HTML.Indexed
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (type (\/), either5)
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
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
import Sexpze.Data.Sexp (Sexp(..), Sexp'(..))
import Sexpze.Data.Sexp.Cursor.Drag (dragFromCursor)
import Sexpze.Utility (unimplemented)
import Web.Event.Event as Event
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type Term = Sexp NodeData AtomData
type Term' = Sexp' NodeData AtomData
type TermSpan = Span NodeData AtomData
type TermZipper = Zipper NodeData AtomData

type NodeData = {}
type AtomData = { label :: String }

defaultNodeData :: NodeData
defaultNodeData = {}

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

type State =
  { termState :: TermState
  , mb_clipboard :: Maybe Clipboard
  }

type TermState =
  { term :: TermSpan
  , cursor :: Cursor
  }

newtype Clipboard = Clipboard TermZipper

derive instance Newtype Clipboard _
derive newtype instance Show Clipboard
derive newtype instance Eq Clipboard

--------------------------------------------------------------------------------
-- component Types
--------------------------------------------------------------------------------

data Query a = KeyboardEvent_Query KeyboardEvent a

newtype Input = Input
  { termState :: TermState
  }

data Output = Updated Term

type Slots :: Row Type
type Slots = ()

type M = Aff
type HM = H.HalogenM State Action Slots Output M

type HTML = H.ComponentHTML Action Slots M

--------------------------------------------------------------------------------
-- Action
--------------------------------------------------------------------------------

data Action
  = Initialize
  | UserAction_Action UserAction

--------------------------------------------------------------------------------
-- UserAction
--------------------------------------------------------------------------------

data UserAction
  = StartDrag_TwoSided PointCursor
  | EndDrag_TwoSided PointCursor
  | UserAction_Core UserAction_Core

derive instance Generic UserAction _

instance Show UserAction where
  show x = genericShow x

data UserAction_Core
  = Delete
  | Copy
  | Paste (Maybe Clipboard)
  | StartDrag PointCursor
  | EndDrag PointCursor

derive instance Generic UserAction_Core _

instance Show UserAction_Core where
  show x = genericShow x

handleUserAction :: UserAction -> HM Unit
handleUserAction = elaborateUserAction >=> traverse_ handleUserAction_Core

elaborateUserAction :: UserAction -> HM (Array UserAction_Core)
elaborateUserAction (StartDrag_TwoSided p1) = do
  -- TODO: way to determine if dragging the left or right half of it?
  pure [ StartDrag p1 ]
elaborateUserAction (EndDrag_TwoSided p1) = do
  state <- get
  let p2 = p1 # shiftPointCursorByPointDist (wrap 1)
  let p = state.termState.term # getCursorHandle state.termState.cursor
  if p1 < p then
    pure [ EndDrag p1 ] -- dragging from right to left
  else
    pure [ EndDrag p2 ] -- dragging from left to right
elaborateUserAction (UserAction_Core action) = pure [ action ]

handleUserAction_Core :: UserAction_Core -> HM Unit
handleUserAction_Core Delete = do
  state <- get
  let cursor /\ term = deleteAtCursor state.termState.cursor state.termState.term
  modify_ _ { termState { cursor = cursor, term = term } }
handleUserAction_Core Copy = unimplemented "handleUserAction_Core" {}
handleUserAction_Core (Paste _) = unimplemented "handleUserAction_Core.Paste" {}
handleUserAction_Core (StartDrag p) = do
  state <- get
  let cursor = Cursor (ZipperCursor (fromPointCursorToZeroWidthSpanCursor p state.termState.term) emptySpanCursor) (Inner Start)
  modify_ _ { termState { cursor = cursor } }
  Console.logShow { cursor }
handleUserAction_Core (EndDrag p) = do
  pure unit

--------------------------------------------------------------------------------
-- rendering
--------------------------------------------------------------------------------

renderTermState :: TermState -> HTML
renderTermState state =
  let
    Cursor c h = state.cursor
  in
    HH.div
      [ HP.classes [ HH.ClassName "TermState" ] ]
      -- (renderTermSpan state.term)
      (renderTermSpanWithCursor (Left c) h mempty state.term)

renderTermSpanWithCursor :: ZipperCursor \/ SpanCursor -> ZipperHandle -> Path -> TermSpan -> Array HTML
renderTermSpanWithCursor c h ph = renderTermWithCursor c h ph <<< fromSpan defaultNodeData

-- TODO: take into account NodeData somehow
renderTermWithCursor :: ZipperCursor \/ SpanCursor -> ZipperHandle -> Path -> Term -> Array HTML
renderTermWithCursor c h ph (Sexp _n es) =
  let
    e = Span es
  in
    unconsZipperCursor c
      # either5
          ( \(_i /\ c') ->
              es
                # mapWithPointIndex
                    (\j' -> [ renderPoint (PointCursor ph j') ])
                    (\i' -> renderTerm'WithCursor (Left c') h ph i')
                # Array.fold
          )
          ( \((d1_outer /\ d2_inner) /\ (_i /\ c')) ->
              let
                j1_outer = shiftPointIndexByPointDist d1_outer (wrap 0)
                j2_outer = shiftPointIndexByPointDistNeg d2_inner (lastPointIndexOfSpan e)
              in
                es
                  # mapWithPointIndex
                      ( \j' ->
                          if j' == j1_outer then
                            [ renderZipperHandle h (Outer Start) (PointCursor ph j1_outer) ]
                          else if j' == j2_outer then
                            [ renderZipperHandle h (Outer End) (PointCursor ph j2_outer) ]
                          else
                            [ renderPoint (PointCursor ph j') ]
                      )
                      (\i' -> renderTerm'WithCursor (Right c') h ph i')
                  # Array.fold
          )
          ( \((d1_outer /\ d2_outer) /\ (d1_inner /\ d2_inner)) ->
              let
                j1_outer = shiftPointIndexByPointDist d1_outer (wrap 0)
                j2_outer = shiftPointIndexByPointDistNeg d2_outer (lastPointIndexOfSpan e)
                j1_inner = shiftPointIndexByPointDist (d1_outer + d1_inner) (wrap 0)
                j2_inner = shiftPointIndexByPointDistNeg (d2_outer + d2_inner) (lastPointIndexOfSpan e)
              in
                es
                  # mapWithPointIndex
                      ( \j' ->
                          -- Inner Start overrides Outer Start
                          if j' == j1_inner then
                            [ renderZipperHandle h (Inner Start) (PointCursor ph j1_inner) ]
                          else if j' == j1_outer then
                            [ renderZipperHandle h (Outer Start) (PointCursor ph j1_outer) ]
                          -- Outer End override s Inner End
                          else if j' == j2_outer then
                            [ renderZipperHandle h (Outer End) (PointCursor ph j2_outer) ]
                          else if j' == j2_inner then
                            [ renderZipperHandle h (Inner End) (PointCursor ph j2_inner) ]
                          else
                            [ renderPoint (PointCursor ph j') ]
                      )
                      (renderTerm' ph)
                  # Array.fold
          )
          ( \(_i /\ c') ->
              es
                # mapWithPointIndex
                    (\j' -> [ renderPoint (PointCursor ph j') ])
                    (\i' -> renderTerm'WithCursor (Right c') h ph i')
                # Array.fold
          )
          ( \(d1_inner /\ d2_inner) ->
              let
                j1_inner = shiftPointIndexByPointDist d1_inner (wrap 0)
                j2_inner = shiftPointIndexByPointDistNeg d2_inner (lastPointIndexOfSpan e)
              in
                es
                  # mapWithPointIndex
                      ( \j' ->
                          if j' == j1_inner then
                            [ renderZipperHandle h (Inner Start) (PointCursor ph j1_inner) ]
                          else if j' == j2_inner then
                            [ renderZipperHandle h (Inner End) (PointCursor ph j2_inner) ]
                          else
                            [ renderPoint (PointCursor ph j') ]
                      )
                      (renderTerm' ph)
                  # Array.fold
          )

renderTerm'WithCursor :: ZipperCursor \/ SpanCursor -> ZipperHandle -> Path -> KidIndex -> Term' -> Array HTML
renderTerm'WithCursor _c _h ph i (Atom a) =
  [ HH.div
      [ HP.classes [ HH.ClassName "Atom" ]
      , HE.onMouseUp \_event -> UserAction_Action $ StartDrag_TwoSided (PointCursor ph (pointIndexRightBeforeKidIndex i))
      , HE.onMouseDown \_event -> UserAction_Action $ StartDrag_TwoSided (PointCursor ph (pointIndexRightAfterKidIndex i))
      ]
      [ HH.text a.label ]
  ]
renderTerm'WithCursor c h ph i (Group e) =
  -- renderTermWithCursor c h (ph `snocPath` i) e
  [ [ HH.div [ HP.classes [ HH.ClassName "Punc", HH.ClassName "Paren", HH.ClassName "LeftParen" ] ] [ HH.text "(" ] ]
  , renderTermWithCursor c h (ph `snocPath` i) e
  , [ HH.div [ HP.classes [ HH.ClassName "Punc", HH.ClassName "Paren", HH.ClassName "RightParen" ] ] [ HH.text ")" ] ]
  ] # Array.fold

mapWithPointIndex :: forall a r. (PointIndex -> r) -> (KidIndex -> a -> r) -> Array a -> Array r
mapWithPointIndex f_point f_kid xs =
  [ [ f_point (wrap 0) ] ] <> (xs # Array.mapWithIndex \i x -> [ f_kid (wrap i) x, f_point (wrap (i + 1)) ])
    # Array.fold

--------------------------------------------------------------------------------

-- TODO: take into account `n : NodeData` somehow
renderTerm :: Path -> Term -> Array HTML
renderTerm ph (Sexp _n es) =
  es
    # mapWithPointIndex
        (\j' -> [ renderPoint (PointCursor ph j') ])
        (\i' -> renderTerm' ph i')
    # Array.fold

renderTermSpan :: Path -> TermSpan -> Array HTML
renderTermSpan ph = renderTerm ph <<< fromSpan defaultNodeData

renderTerm' :: Path -> KidIndex -> Term' -> Array HTML
renderTerm' ph i (Group e) =
  [ [ HH.div [ HP.classes [ HH.ClassName "Punc", HH.ClassName "Paren", HH.ClassName "LeftParen" ] ] [ HH.text "(" ] ]
  , renderTerm (ph `snocPath` i) e
  , [ HH.div [ HP.classes [ HH.ClassName "Punc", HH.ClassName "Paren", HH.ClassName "RightParen" ] ] [ HH.text ")" ] ]
  ] # Array.fold
renderTerm' ph i (Atom a) =
  [ HH.div
      [ HP.classes [ HH.ClassName "Atom" ]
      , HE.onMouseUp \_event -> UserAction_Action $ StartDrag_TwoSided (PointCursor ph (pointIndexRightBeforeKidIndex i))
      , HE.onMouseDown \_event -> UserAction_Action $ StartDrag_TwoSided (PointCursor ph (pointIndexRightAfterKidIndex i))
      ]
      [ HH.text a.label ]
  ]

--------------------------------------------------------------------------------

renderPoint :: PointCursor -> HTML
renderPoint p = renderAnchor p [ HP.classes [ HH.ClassName "Anchor", HH.ClassName "Point" ] ] [ HH.text "•" ]

renderZipperHandle :: ZipperHandle -> ZipperHandle -> PointCursor -> HTML
renderZipperHandle h h'@(Outer Start) p = renderAnchor p [ HP.classes ([ [ HH.ClassName "Anchor", HH.ClassName "ZipperHandle", HH.ClassName "OuterStart" ], if h == h' then [ HH.ClassName "active" ] else [] ] # Array.fold) ] [ HH.text "[" ]
renderZipperHandle h h'@(Outer End) p = renderAnchor p [ HP.classes ([ [ HH.ClassName "Anchor", HH.ClassName "ZipperHandle", HH.ClassName "OuterEnd" ], if h == h' then [ HH.ClassName "active" ] else [] ] # Array.fold) ] [ HH.text "]" ]
renderZipperHandle h h'@(Inner Start) p = renderAnchor p [ HP.classes ([ [ HH.ClassName "Anchor", HH.ClassName "ZipperHandle", HH.ClassName "InnerStart" ], if h == h' then [ HH.ClassName "active" ] else [] ] # Array.fold) ] [ HH.text "{" ]
renderZipperHandle h h'@(Inner End) p = renderAnchor p [ HP.classes ([ [ HH.ClassName "Anchor", HH.ClassName "ZipperHandle", HH.ClassName "InnerEnd" ], if h == h' then [ HH.ClassName "active" ] else [] ] # Array.fold) ] [ HH.text "}" ]

renderAnchor :: PointCursor -> Array (HH.IProp HTML.Indexed.HTMLdiv Action) -> Array HTML -> HTML
renderAnchor p props =
  HH.div
    ( [ HE.onMouseDown \_event -> UserAction_Action $ UserAction_Core $ StartDrag p
      , HE.onMouseUp \_event -> UserAction_Action $ UserAction_Core $ EndDrag p
      ] <> props
    )

--------------------------------------------------------------------------------
-- component
--------------------------------------------------------------------------------

component :: H.Component Query Input Output M
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState (Input input) =
    { termState: input.termState
    , mb_clipboard: empty
    }

  eval = H.mkEval H.defaultEval { handleAction = handleAction, handleQuery = handleQuery }

  handleQuery :: forall a. Query a -> HM (Maybe a)
  handleQuery = case _ of
    KeyboardEvent_Query event a -> do
      Console.log $ "[KeyboardEvent_Query] " <> show { key: KeyboardEvent.key event }
      let actions = event # parseKeyboardEvent
      when (not (Array.null actions)) do
        event # KeyboardEvent.toEvent # Event.preventDefault # liftEffect
      actions # traverse_ handleUserAction
      pure (Just a)

  handleAction :: Action -> HM Unit
  handleAction Initialize = pure unit
  handleAction (UserAction_Action action) = do
    handleUserAction action
    pure unit

  render state =
    HH.div
      [ HP.classes [ HH.ClassName "Editor" ] ]
      [ renderTermState state.termState ]

parseKeyboardEvent :: KeyboardEvent -> Array UserAction
parseKeyboardEvent event =
  if cmd && key == "c" then [ UserAction_Core Copy ]
  else if cmd && key == "x" then [ UserAction_Core Copy, UserAction_Core Delete ]
  else if cmd && key == "v" then [ UserAction_Core $ Paste empty ]
  else if key == "Backspace" then [ UserAction_Core Delete ]
  else if key == "(" || key == ")" then [ UserAction_Core $ Paste (pure (Clipboard (Zipper (Span [ Group (Sexp defaultNodeData []) ]) (PointCursor (wrap 0 `consPath` mempty) (wrap 0))))) ]
  else if String.length key == 1 then [ UserAction_Core $ Paste (pure (Clipboard (Zipper (Span [ Atom { label: key } ]) (PointCursor mempty (wrap 1))))) ]
  else Debug.trace (show { key }) \_ -> []
  where
  key = event # KeyboardEvent.key
  shift = event # KeyboardEvent.shiftKey
  cmd = (event # KeyboardEvent.ctrlKey) || (event # KeyboardEvent.metaKey)
