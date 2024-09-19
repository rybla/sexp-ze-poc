module Sexpze.Component.Editor where

import Prelude
import Sexpze.Data.Sexp.Cursor

import Control.Alt ((<|>))
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
import Sexpze.Utility (allEqual, todo, unimplemented)
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
  , mb_clipboard :: Maybe Fragment
  }

type TermState =
  { term :: TermSpan
  , cursor :: Cursor
  }

newtype Fragment = Fragment TermZipper

derive instance Newtype Fragment _
derive newtype instance Show Fragment
derive newtype instance Eq Fragment

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
  | Paste (Maybe Fragment) -- can supply a clipboard to override the clipboard in the editor state
  | StartDrag PointCursor
  | EndDrag PointCursor

derive instance Generic UserAction_Core _

instance Show UserAction_Core where
  show x = genericShow x

handleUserAction :: UserAction -> HM Unit
handleUserAction = elaborateUserAction >=> traverse_ handleUserAction_Core

elaborateUserAction :: UserAction -> HM (Array UserAction_Core)
elaborateUserAction (StartDrag_TwoSided p1) = do
  -- TODO: make this actually work
  -- TODO: way to determine if dragging the left or right half of it?
  -- pure [ StartDrag p1 ]
  pure []
elaborateUserAction (EndDrag_TwoSided p1) = do
  -- TODO: make this actually work
  -- state <- get
  -- let p2 = p1 # shiftPointCursorByPointDist (wrap 1)
  -- let p = state.termState.term # getCursorHandle state.termState.cursor
  -- if p == p1 then
  --   pure [ EndDrag p1 ] -- started on p1
  -- else if p == p2 then
  --   pure [ EndDrag p1 ] -- started on p2
  -- else if p < p1 then
  --   pure [ EndDrag p2 ] -- dragging left to right
  -- else if p2 < p then
  --   pure [ EndDrag p1 ] -- dragging right to left
  -- else
  --   pure []
  pure []
elaborateUserAction (UserAction_Core action) = pure [ action ]

handleUserAction_Core :: UserAction_Core -> HM Unit
handleUserAction_Core Delete = do
  state <- get
  let cursor /\ term = deleteAtCursor state.termState.cursor state.termState.term
  modify_ _ { termState { cursor = cursor, term = term } }
handleUserAction_Core Copy = unimplemented "handleUserAction_Core" {}
handleUserAction_Core (Paste mb_clipboard) = do
  state <- get
  case mb_clipboard <|> state.mb_clipboard of
    Nothing -> pure unit
    Just (Fragment clipboard) -> do
      let cursor' /\ term' = state.termState.term # insertAtCursor clipboard state.termState.cursor
      modify_ _ { termState { cursor = cursor', term = term' } }
handleUserAction_Core (StartDrag p) = do
  state <- get
  let cursor = Cursor (ZipperCursor (fromPointCursorToZeroWidthSpanCursor p state.termState.term) (SpanCursor mempty zero zero)) (Inner Start)
  modify_ _ { termState { cursor = cursor } }
  -- Console.log ("[StartDrag] " <> show { cursor })
  pure unit
handleUserAction_Core (EndDrag p) = do
  state <- get
  let cursor = dragFromCursor state.termState.cursor p state.termState.term
  modify_ _ { termState { cursor = cursor } }
  Console.log ("[EndDrag] " <> show { cursor })

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
          ( \(i /\ c') ->
              es
                # mapWithPointIndex
                    (\j' -> [ renderPoint (PointCursor ph j') ])
                    (\i' -> if i' == i then renderTerm'WithCursor (Left c') h ph i' else renderTerm' ph i')
                # Array.fold
          )
          ( \((d1_outer /\ d2_inner) /\ (i /\ c')) ->
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
                      (\i' -> if i' == i then renderTerm'WithCursor (Right c') h ph i' else renderTerm' ph i')
                  # Array.fold
          )
          ( \((d1_outer /\ d2_outer) /\ (d1_inner /\ d2_inner)) ->
              let
                -- _ = Debug.trace (show { j1_outer, j2_outer, j1_inner, j2_inner })
                j1_outer = shiftPointIndexByPointDist d1_outer (wrap 0)
                j2_outer = shiftPointIndexByPointDistNeg d2_outer (lastPointIndexOfSpan e)
                j1_inner = shiftPointIndexByPointDist (d1_outer + d1_inner) (wrap 0)
                j2_inner = shiftPointIndexByPointDistNeg (d2_outer + d2_inner) (lastPointIndexOfSpan e)
              in
                -- point
                if j1_outer == j2_outer then
                  es
                    # mapWithPointIndex
                        ( \j' ->
                            if j' == j1_inner then
                              [ renderPointHandle (PointCursor ph j1_inner) ]
                            else
                              [ renderPoint (PointCursor ph j') ]
                        )
                        (renderTerm' ph)
                    # Array.fold
                -- span from start
                else if allEqual [ j1_inner, j2_inner, j2_outer ] then
                  let
                    h' = case h of
                      Outer Start -> Start
                      _ -> End
                  in
                    es
                      # mapWithPointIndex
                          ( \j' ->
                              if j' == j1_outer then
                                [ renderSpanHandle h' Start (PointCursor ph j1_outer) ]
                              else if j' == j1_inner then
                                [ renderSpanHandle h' End (PointCursor ph j1_inner) ]
                              else
                                [ renderPoint (PointCursor ph j') ]
                          )
                          (renderTerm' ph)
                      # Array.fold
                -- span from end
                else if allEqual [ j1_outer, j1_inner, j2_outer ] then
                  let
                    h' = case h of
                      Outer End -> End
                      _ -> Start
                  in
                    es
                      # mapWithPointIndex
                          ( \j' ->
                              if j' == j2_outer then
                                [ renderSpanHandle h' Start (PointCursor ph j1_outer) ]
                              else if j' == j1_outer then
                                [ renderSpanHandle h' End (PointCursor ph j1_outer) ]
                              else
                                [ renderPoint (PointCursor ph j') ]
                          )
                          (renderTerm' ph)
                      # Array.fold
                else
                  es
                    # mapWithPointIndex
                        ( \j' ->
                            if j' == j1_outer then
                              [ renderZipperHandle h (Outer Start) (PointCursor ph j1_outer) ]
                            else if j' == j1_inner then
                              [ renderZipperHandle h (Inner Start) (PointCursor ph j1_inner) ]
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
          ( \(i /\ c') ->
              es
                # mapWithPointIndex
                    (\j' -> [ renderPoint (PointCursor ph j') ])
                    (\i' -> if i' == i then renderTerm'WithCursor (Right c') h ph i' else renderTerm' ph i')
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
                          if j' == j1_inner && j' == j2_inner then
                            [ renderZipperHandleInnerMiddle h (PointCursor ph j1_inner) ]
                          else if j' == j1_inner then
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
      , HE.onMouseDown \_event -> UserAction_Action $ EndDrag_TwoSided (PointCursor ph (pointIndexRightAfterKidIndex i))
      ]
      [ HH.text a.label ]
  ]
renderTerm'WithCursor c h ph i (Group e) =
  -- renderTermWithCursor c h (ph `snocPath` i) e
  [ [ HH.div [ HP.classes [ HH.ClassName "Punc", HH.ClassName "Paren", HH.ClassName "LeftParen" ] ] [ HH.text "(" ] ]
  , renderTermWithCursor c h (ph `snocPath` i) e
  , [ HH.div [ HP.classes [ HH.ClassName "Punc", HH.ClassName "Paren", HH.ClassName "RightParen" ] ] [ HH.text ")" ] ]
  ] # Array.fold

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
      , HE.onMouseDown \_event -> UserAction_Action $ EndDrag_TwoSided (PointCursor ph (pointIndexRightAfterKidIndex i))
      ]
      [ HH.text a.label ]
  ]

--------------------------------------------------------------------------------

renderPoint :: PointCursor -> HTML
renderPoint p = renderAnchor p [ HP.classes [ HH.ClassName "Anchor", HH.ClassName "Point" ] ] [ HH.text "•" ]

renderZipperHandle :: ZipperHandle -> ZipperHandle -> PointCursor -> HTML
renderZipperHandle h h'@(Outer Start) p = renderAnchor p [ HP.classes ([ [ HH.ClassName "Anchor", HH.ClassName "Handle", HH.ClassName "ZipperHandle", HH.ClassName "OuterStart" ], if h == h' then [ HH.ClassName "active" ] else [] ] # Array.fold) ] [ HH.text "⦃" ]
renderZipperHandle h h'@(Outer End) p = renderAnchor p [ HP.classes ([ [ HH.ClassName "Anchor", HH.ClassName "Handle", HH.ClassName "ZipperHandle", HH.ClassName "OuterEnd" ], if h == h' then [ HH.ClassName "active" ] else [] ] # Array.fold) ] [ HH.text "⦄" ]
renderZipperHandle h h'@(Inner Start) p = renderAnchor p [ HP.classes ([ [ HH.ClassName "Anchor", HH.ClassName "Handle", HH.ClassName "ZipperHandle", HH.ClassName "InnerStart" ], if h == h' then [ HH.ClassName "active" ] else [] ] # Array.fold) ] [ HH.text "{" ]
renderZipperHandle h h'@(Inner End) p = renderAnchor p [ HP.classes ([ [ HH.ClassName "Anchor", HH.ClassName "Handle", HH.ClassName "ZipperHandle", HH.ClassName "InnerEnd" ], if h == h' then [ HH.ClassName "active" ] else [] ] # Array.fold) ] [ HH.text "}" ]

renderZipperHandleInnerMiddle :: ZipperHandle -> PointCursor -> HTML
renderZipperHandleInnerMiddle h p = renderAnchor p
  [ HP.classes
      ( [ [ HH.ClassName "Anchor", HH.ClassName "Handle", HH.ClassName "ZipperHandle", HH.ClassName "InnerMiddle" ]
        , case h of
            Outer _ -> []
            Inner _ -> [ HH.ClassName "active" ]
        ] # Array.fold
      )
  ]
  [ HH.text "|" ]

renderSpanHandle :: SpanHandle -> SpanHandle -> PointCursor -> HTML
renderSpanHandle h h'@Start p = renderAnchor p [ HP.classes ([ [ HH.ClassName "Anchor", HH.ClassName "Handle", HH.ClassName "SpanHandle", HH.ClassName "Start" ], if h == h' then [ HH.ClassName "active" ] else [] ] # Array.fold) ] [ HH.text "[" ]
renderSpanHandle h h'@End p = renderAnchor p [ HP.classes ([ [ HH.ClassName "Anchor", HH.ClassName "Handle", HH.ClassName "SpanHandle", HH.ClassName "End" ], if h == h' then [ HH.ClassName "active" ] else [] ] # Array.fold) ] [ HH.text "]" ]

renderPointHandle :: PointCursor -> HTML
renderPointHandle p = renderAnchor p [ HP.classes [ HH.ClassName "Anchor", HH.ClassName "Handle", HH.ClassName "PointHandle", HH.ClassName "active" ] ] [ HH.text "|" ]

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
      -- Console.log $ "[KeyboardEvent_Query] " <> show { key: KeyboardEvent.key event }
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
  else if key == "(" || key == ")" then [ UserAction_Core $ Paste (pure (Fragment (Zipper (Span [ Group (Sexp defaultNodeData []) ]) (PointCursor (wrap 0 `consPath` mempty) (wrap 0))))) ]
  else if String.length key == 1 then [ UserAction_Core $ Paste (pure (Fragment (Zipper (Span [ Atom { label: key } ]) (PointCursor mempty (wrap 1))))) ]
  else Debug.trace (show { key }) \_ -> []
  where
  key = event # KeyboardEvent.key
  shift = event # KeyboardEvent.shiftKey
  cmd = (event # KeyboardEvent.ctrlKey) || (event # KeyboardEvent.metaKey)
