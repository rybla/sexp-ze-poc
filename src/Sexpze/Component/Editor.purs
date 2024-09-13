module Sexpze.Component.Editor where

import Prelude

import Control.Monad.State (get, modify_)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (either5)
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sexpze.Data.Sexp (Sexp, Sexp'(..))
import Sexpze.Data.Sexp.Cursor (Cursor(..), Path, Point(..), SexpKidIndex, SpanCursor, SpanHandle(..), ZipperHandle(..), ZipperOrSpanCursor, dragFromPoint, mapWithSexpPointIndex, orderPoints, unconsPoint, unconsSpanCursor, unconsZipperCursor)
import Sexpze.Utility (todo)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type Term = Sexp TermData String
type Term' = Sexp' TermData String

type TermData = {}

--------------------------------------------------------------------------------
-- component
--------------------------------------------------------------------------------

data Query a = KeyboardEvent_Query KeyboardEvent a

newtype Input = Input
  { term :: Term
  , cursor :: Cursor
  }

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
  initialState (Input input) =
    { term: input.term
    , cursor: input.cursor
    }

  eval = H.mkEval H.defaultEval { handleAction = handleAction }

  handleAction :: Action -> HM Unit
  handleAction = case _ of
    UserAction_Action action _config -> do
      handleUserAction action
      pure unit

  render state =
    HH.div
      [ HP.classes [ HH.ClassName "Editor" ] ]
      [ HH.div []
          [ HH.div [ HP.classes [ HH.ClassName "Term" ] ] (renderTermWithCursor state.cursor state.term) ]
      ]

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

type State =
  { term :: Term
  , cursor :: Cursor
  }

--------------------------------------------------------------------------------
-- Action
--------------------------------------------------------------------------------

data Action = UserAction_Action UserAction ActionConfig

derive instance Generic Action _

instance Show Action where
  show x = genericShow x

data ActionConfig = MouseActionConfig
  { event :: MouseEvent
  }

instance Show ActionConfig where
  show _ = "ActionConfig { ... }"

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
  | StartDrag_double Point Point
  | EndDrag Point
  | EndDrag_double Point Point
  | InsertAtom String
  | InsertGroup

derive instance Generic UserAction _

instance Show UserAction where
  show x = genericShow x

handleUserAction :: UserAction -> HM Unit
handleUserAction MoveLeft = todo "handleUserAction" {}
handleUserAction MoveRight = todo "handleUserAction" {}
handleUserAction SelectLeft = todo "handleUserAction" {}
handleUserAction SelectRight = todo "handleUserAction" {}
handleUserAction Delete = todo "handleUserAction" {}
handleUserAction Copy = todo "handleUserAction" {}
handleUserAction Paste = todo "handleUserAction" {}
handleUserAction (StartDrag p) = do
  modify_ _ { cursor = InjectPoint p }
handleUserAction (StartDrag_double p1 p1') = do
  modify_ _ { cursor = InjectPoint p1 } -- TODO
handleUserAction (EndDrag p2) = do
  { cursor, term } <- get
  case cursor of
    InjectPoint p1 -> do
      modify_ _ { cursor = dragFromPoint p1 p2 term }
    _ -> pure unit -- TODO
handleUserAction (EndDrag_double p2 p2') = do
  { cursor, term } <- get
  case cursor of
    InjectPoint p1 -> do
      case orderPoints p1 p2' of
        LT /\ _ -> modify_ _ { cursor = dragFromPoint p1 p2' term }
        _ -> modify_ _ { cursor = dragFromPoint p1 p2 term }
    _ -> pure unit -- TODO
handleUserAction (InsertAtom _) = todo "handleUserAction" {}
handleUserAction InsertGroup = todo "handleUserAction" {}

--------------------------------------------------------------------------------
-- renderTermWithCursor
--------------------------------------------------------------------------------

renderTermWithCursor :: Cursor -> Term -> Array HTML
renderTermWithCursor (InjectPoint p) = renderTermWithPoint p mempty
renderTermWithCursor (InjectSpanCursor s sh) = renderTermWithSpanCursor s sh mempty
renderTermWithCursor (InjectZipperCursor z zh) = renderTermWithZipperCursor (Left z) zh mempty

--------------------------------------------------------------------------------
-- renderTermWithPoint
--------------------------------------------------------------------------------

renderTermWithPoint :: Point -> Path -> Term -> Array HTML
renderTermWithPoint p ph =
  mapWithSexpPointIndex
    ( case unconsPoint p of
        Left _ -> \j' -> [ renderPointHandle (Point ph j') [ HH.ClassName "Space" ] "•" ]
        Right j -> \j' ->
          if j == j' then
            [ renderPointHandle (Point ph j')
                [ H.ClassName "Cursor", H.ClassName "active" ]
                "|"
            ]
          else
            [ renderPointHandle (Point ph j') [ HH.ClassName "Space" ] "•" ]
    )
    ( case unconsPoint p of
        Left (i /\ p') -> \i' ->
          if i == i' then
            renderTerm'_helper (renderTermWithPoint p') ph i'
          else
            renderTerm' ph i'
        Right _ -> renderTerm' ph
    )
    >>> Array.fold

renderTerm'_helper :: (Path -> Term -> Array HTML) -> Path -> SexpKidIndex -> Term' -> Array HTML
renderTerm'_helper _ ph i (Atom a) =
  [ HH.div
      ( [ [ HP.classes [ HH.ClassName "Atom" ] ]
        , -- an Atom is a Point handle for the Point right _before_ it
          pointHandleProps (Point ph (wrap (unwrap i)))
        ] # Array.fold
      )
      [ HH.text a ]
  ]
renderTerm'_helper f ph i (Group _n xs) =
  [ -- a Group's left paren is a Point handle for the Point right _before_ it
    [ renderPointHandle (Point ph (wrap (unwrap i))) [ HH.ClassName "Paren", HH.ClassName "OpenParen" ] "(" ]
  , f (ph `List.snoc` i) xs
  , -- a Group's right paren is a Point handle for the Point right _after_ it
    [ renderPointHandle (Point ph (wrap (unwrap i + 1))) [ HH.ClassName "Paren", HH.ClassName "CloseParen" ] ")" ]
  ] # Array.fold

--------------------------------------------------------------------------------
-- renderTermWithSpanCursor
--------------------------------------------------------------------------------

renderTermWithSpanCursor :: SpanCursor -> SpanHandle -> Path -> Term -> Array HTML
renderTermWithSpanCursor s sh ph =
  mapWithSexpPointIndex
    ( case unconsSpanCursor s of
        Left (j1 /\ j2) -> \j' ->
          if j1 == j' then
            [ renderPointHandle (Point ph j')
                ( [ [ H.ClassName "Cursor" ]
                  , if sh == StartSpanHandle then [ H.ClassName "active" ] else []
                  ] # Array.fold
                )
                "["
            ]
          else if j2 == j' then
            [ renderPointHandle (Point ph j')
                ( [ [ H.ClassName "Cursor" ]
                  , if sh == EndSpanHandle then [ H.ClassName "active" ] else []
                  ] # Array.fold
                )
                "]"
            ]
          else
            [ renderPointHandle (Point ph j') [ HH.ClassName "Space" ] "•" ]
        Right _ -> \j' -> [ renderPointHandle (Point ph j') [ HH.ClassName "Space" ] "•" ]
    )
    ( case unconsSpanCursor s of
        Left _ -> renderTerm' ph
        Right (i /\ s') -> \i' ->
          if i == i' then
            renderTerm'_helper (renderTermWithSpanCursor s' sh) ph i'
          else
            renderTerm' ph i'
    )
    >>> Array.fold

--------------------------------------------------------------------------------
-- renderTermWithZipperCursor
--------------------------------------------------------------------------------

renderTermWithZipperCursor :: ZipperOrSpanCursor -> ZipperHandle -> Path -> Term -> Array HTML
renderTermWithZipperCursor zos zh ph =
  mapWithSexpPointIndex
    -- points
    ( unconsZipperCursor zos # either5
        ( \(_i /\ _zos') j ->
            [ renderPointHandle (Point ph j) [ HH.ClassName "Space" ] "•" ]
        )
        ( \((j1 /\ j2) /\ _i /\ _s') j' ->
            if j1 == j' then
              [ renderPointHandle (Point ph j')
                  ( [ [ H.ClassName "Cursor" ]
                    , if zh == OuterStartZipperHandle then [ H.ClassName "active" ] else []
                    ] # Array.fold
                  )
                  "{"
              ]
            else if j2 == j' then
              [ renderPointHandle (Point ph j')
                  ( [ [ H.ClassName "Cursor" ]
                    , if zh == OuterEndZipperHandle then [ H.ClassName "active" ] else []
                    ] # Array.fold
                  )
                  "}"
              ]
            else
              [ renderPointHandle (Point ph j') [ HH.ClassName "Space" ] "•" ]
        )
        ( \((j1 /\ j2) /\ (j1' /\ j2')) j' ->
            if j1 == j' then
              [ renderPointHandle (Point ph j')
                  ( [ [ H.ClassName "Cursor" ]
                    , if zh == OuterStartZipperHandle then [ H.ClassName "active" ] else []
                    ] # Array.fold
                  )
                  "{"
              ]
            else if j2 == j' then
              [ renderPointHandle (Point ph j')
                  ( [ [ H.ClassName "Cursor" ]
                    , if zh == OuterEndZipperHandle then [ H.ClassName "active" ] else []
                    ] # Array.fold
                  )
                  "}"
              ]
            else if j1' == j' then
              [ renderPointHandle (Point ph j')
                  ( [ [ H.ClassName "Cursor" ]
                    , if zh == InnerStartZipperHandle then [ H.ClassName "active" ] else []
                    ] # Array.fold
                  )
                  "{"
              ]
            else if j2' == j' then
              [ renderPointHandle (Point ph j')
                  ( [ [ H.ClassName "Cursor" ]
                    , if zh == InnerEndZipperHandle then [ H.ClassName "active" ] else []
                    ] # Array.fold
                  )
                  "}"
              ]
            else
              [ renderPointHandle (Point ph j') [ HH.ClassName "Space" ] "•" ]
        )
        ( \(_i /\ _zos') j ->
            [ renderPointHandle (Point ph j) [ HH.ClassName "Space" ] "•" ]
        )
        ( \(j1 /\ j2) j' ->
            if j1 == j' then
              [ renderPointHandle (Point ph j')
                  ( [ [ H.ClassName "Cursor" ]
                    , if zh == InnerStartZipperHandle then [ H.ClassName "active" ] else []
                    ] # Array.fold
                  )
                  "{"
              ]
            else if j2 == j' then
              [ renderPointHandle (Point ph j')
                  ( [ [ H.ClassName "Cursor" ]
                    , if zh == InnerEndZipperHandle then [ H.ClassName "active" ] else []
                    ] # Array.fold
                  )
                  "}"
              ]
            else
              [ renderPointHandle (Point ph j') [ HH.ClassName "Space" ] "•" ]
        )
    )
    -- kids
    ( unconsZipperCursor zos # either5
        ( \(i /\ zos') -> \i' ->
            if i == i' then
              renderTerm'_helper (renderTermWithZipperCursor zos' zh) ph i'
            else
              renderTerm' ph i'
        )
        ( \((_j1 /\ _j2) /\ i /\ zos') -> \i' ->
            if i == i' then
              renderTerm'_helper (renderTermWithZipperCursor zos' zh) ph i'
            else
              renderTerm' ph i'
        )
        (\((_j1 /\ _j2) /\ (_j1' /\ _j2')) -> renderTerm' ph)
        ( \(i /\ zos') -> \i' ->
            if i == i' then
              renderTerm'_helper (renderTermWithZipperCursor zos' zh) ph i'
            else
              renderTerm' ph i'
        )
        (\(_j1 /\ _j2) -> renderTerm' ph)
    )
    >>> Array.fold

--------------------------------------------------------------------------------
-- renderTerm
--------------------------------------------------------------------------------

renderTerm :: Path -> Term -> Array HTML
renderTerm ph =
  mapWithSexpPointIndex
    (\j -> [ renderPointHandle (Point ph j) [ HH.ClassName "Space" ] "•" ])
    (\i -> renderTerm' ph i)
    >>> Array.fold

renderTerm' :: Path -> SexpKidIndex -> Term' -> Array HTML
renderTerm' ph i (Atom a) =
  [ HH.div
      ( [ [ HP.classes [ HH.ClassName "Atom" ] ]
        , -- an Atom is a Point handle for the Point right _before_ it
          pointHandleProps_double (Point ph (wrap (unwrap i))) (Point ph (wrap (unwrap i + 1)))
        ] # Array.fold
      )
      [ HH.text a ]
  ]
renderTerm' ph i (Group _n xs) =
  [ -- a Group's left paren is a Point handle for the Point right _before_ it
    [ renderPointHandle (Point ph (wrap (unwrap i))) [ HH.ClassName "Paren", HH.ClassName "OpenParen" ] "(" ]
  , renderTerm (ph `List.snoc` i) xs
  , -- a Group's right paren is a Point handle for the Point right _after_ it
    [ renderPointHandle (Point ph (wrap (unwrap i + 1))) [ HH.ClassName "Paren", HH.ClassName "CloseParen" ] ")" ]
  ] # Array.fold

renderPointHandle :: Point -> Array H.ClassName -> String -> HTML
renderPointHandle p cns s =
  HH.div
    ( [ [ HP.classes ([ HH.ClassName "PointHandle" ] <> cns) ]
      , pointHandleProps p
      ] # Array.fold
    )
    [ HH.text s ]

pointHandleProps :: forall r. Point -> Array (HH.IProp (onMouseDown :: MouseEvent, onMouseUp :: MouseEvent | r) Action)
pointHandleProps p =
  [ HE.onMouseDown (\event -> UserAction_Action (StartDrag p) $ MouseActionConfig { event })
  , HE.onMouseUp (\event -> UserAction_Action (EndDrag p) $ MouseActionConfig { event })
  ]

pointHandleProps_double :: forall r. Point -> Point -> Array (HH.IProp (onMouseDown :: MouseEvent, onMouseUp :: MouseEvent | r) Action)
pointHandleProps_double p1 p2 =
  [ HE.onMouseDown (\event -> UserAction_Action (StartDrag_double p1 p2) $ MouseActionConfig { event })
  , HE.onMouseUp (\event -> UserAction_Action (EndDrag_double p1 p2) $ MouseActionConfig { event })
  ]
