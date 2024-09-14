module Sexpze.Component.Editor where

import Prelude

import Control.Monad.State (get, modify_)
import Control.Plus (empty)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (either5)
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype as NT
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Debug as Debug
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sexpze.Data.Sexp (Sexp, Sexp'(..))
import Sexpze.Data.Sexp.Cursor (Cursor(..), Path, Point(..), SexpKidIndex, SpanCursor(..), SpanHandle(..), Zipper(..), ZipperCursor(..), ZipperHandle(..), ZipperOrSpanCursor, atPoint, atSpanCursor, atZipperCursor, dragFromPoint, getSpanHandle, getZipperHandle, mapWithSexpPointIndex, nestPaths, orderPoints, unconsPoint, unconsSpanCursor, unconsZipperCursor)
import Sexpze.Utility (todo)
import Web.Event.Event as Event
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.MouseEvent (MouseEvent)

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type Term = Sexp TermData String
type Term' = Sexp' TermData String
type TermZipper = Zipper TermData String

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
  handleAction (UserAction_Action action _config) = do
    handleUserAction action
    pure unit

  render state =
    HH.div
      [ HP.classes [ HH.ClassName "Editor" ] ]
      [ HH.div [ HP.classes [ HH.ClassName "Term" ] ] (renderTermWithCursor state.cursor state.term)
      ]

parseKeyboardEvent :: KeyboardEvent -> Array UserAction
parseKeyboardEvent event =
  if cmd && key == "c" then [ Copy ]
  else if cmd && key == "x" then [ Copy, Delete ]
  else if cmd && key == "v" then [ Paste empty ]
  else if key == "Backspace" then [ Delete ]
  else if key == "(" || key == ")" then [ Paste (pure (ZipperClipboard (Zipper [ Group {} [] ] (Point mempty (NT.wrap 0))))) ]
  else if String.length key == 1 then [ Paste (pure (SpanClipboard [ Atom key ])) ]
  else Debug.trace (show { key }) \_ -> []
  where
  key = event # KeyboardEvent.key
  shift = event # KeyboardEvent.shiftKey
  cmd = (event # KeyboardEvent.ctrlKey) || (event # KeyboardEvent.metaKey)

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

type State =
  { term :: Term
  , cursor :: Cursor
  , mb_clipboard :: Maybe Clipboard
  }

data Clipboard
  = SpanClipboard Term
  | ZipperClipboard TermZipper

derive instance Generic Clipboard _

instance Show Clipboard where
  show x = genericShow x

--------------------------------------------------------------------------------
-- Action
--------------------------------------------------------------------------------

data Action
  = Initialize
  | UserAction_Action UserAction ActionConfig

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
  | Paste (Maybe Clipboard)
  | StartDrag Point
  | StartDrag_double Point Point
  | EndDrag Point
  | EndDrag_double Point Point

derive instance Generic UserAction _

instance Show UserAction where
  show x = genericShow x

handleUserAction :: UserAction -> HM Unit
handleUserAction MoveLeft = todo "handleUserAction" {}
handleUserAction MoveRight = todo "handleUserAction" {}
handleUserAction SelectLeft = todo "handleUserAction" {}
handleUserAction SelectRight = todo "handleUserAction" {}
handleUserAction Delete = do
  state <- get
  case state.cursor of
    InjectPoint (Point ph j) -> do
      unless (j == NT.wrap 0) do
        modify_ _
          { term = state.term # atSpanCursor (SpanCursor ph (NT.wrap (NT.unwrap j - 1)) j) # fst # (_ $ [])
          , cursor = InjectPoint $ Point ph (NT.wrap (NT.unwrap j - 1))
          }
    InjectSpanCursor s _ -> do
      modify_ _
        { term = state.term # atSpanCursor s # fst # (_ $ [])
        , cursor = InjectPoint $ getSpanHandle StartSpanHandle s
        }
    InjectZipperCursor z@(ZipperCursor s1 s2) _ -> do
      modify_ _
        { term = state.term # atSpanCursor s1 # \(wrap1 /\ xs1) -> wrap1 $ xs1 # atSpanCursor s2 # \(_wrap2 /\ xs2) -> xs2
        , cursor = InjectPoint $ getZipperHandle OuterStartZipperHandle z
        }
handleUserAction Copy = do
  state <- get
  case state.cursor of
    InjectPoint _ -> pure unit
    InjectSpanCursor s _ -> do
      modify_ _ { mb_clipboard = state.term # atSpanCursor s # snd # SpanClipboard # pure }
    InjectZipperCursor z _ -> do
      modify_ _ { mb_clipboard = state.term # atZipperCursor z # snd # ZipperClipboard # pure }
handleUserAction (Paste mb_clipboard_) = do
  state <- get
  let
    mb_clipboard = case mb_clipboard_ of
      Nothing -> state.mb_clipboard
      Just clipboard -> pure clipboard
  case state.cursor /\ mb_clipboard of
    _ /\ Nothing -> pure unit
    -- splice into point cursor with span cursor
    -- cursor ends up as a point at end of pasted span
    InjectPoint p@(Point ph j) /\ Just (SpanClipboard xs_cb) -> modify_ _
      { term = state.term # atPoint p # (\f -> f xs_cb)
      , cursor = InjectPoint $ Point ph (NT.wrap (NT.unwrap j + Array.length xs_cb))
      }
    -- splice into point cursor with zipper cursor
    -- cursor ends up at InnerStartZipperHandle
    InjectPoint p@(Point ph j) /\ Just (ZipperClipboard (Zipper xs_cb p_cb@(Point ph_cb j_cb))) -> modify_ _
      { term = state.term # atPoint p # (_ $ xs_cb # atPoint p_cb # (_ $ []))
      , cursor = InjectPoint $ Point ((ph `List.snoc` NT.wrap (NT.unwrap j)) `nestPaths` ph_cb) j_cb
      }
    -- replace span cursor with span clipboard
    -- cursor ends up at EndSpanHandle
    InjectSpanCursor s@(SpanCursor ph j1 _) _ /\ Just (SpanClipboard xs_cb) -> modify_ _
      { term = state.term # atSpanCursor s # snd # const xs_cb
      , cursor = InjectPoint $ Point ph (NT.wrap (NT.unwrap j1 + Array.length xs_cb))
      }
    -- wrap span cursor with zipper clibpoard
    -- cursr ends up at InnerStartZipperHandle
    InjectSpanCursor sc@(SpanCursor ph j1 _) _ /\ Just (ZipperClipboard (Zipper xs_cb p_cb@(Point ph_cb j_cb))) -> modify_ _
      { term = state.term # atSpanCursor sc # \(_sc /\ xs_sc) -> xs_cb # atPoint p_cb # (_ $ xs_sc)
      , cursor = InjectPoint $ Point (ph <> List.singleton (NT.wrap (NT.unwrap j1)) `nestPaths` ph_cb) j_cb
      }
    -- can't paste at a zipper cursor with a span clibboard
    InjectZipperCursor _zc _ /\ Just (SpanClipboard _x_cb) -> pure unit
    -- replace zipper crsor with zipper clibpoard
    InjectZipperCursor zc@(ZipperCursor (SpanCursor ph1_zc _ _) _) _ /\ Just (ZipperClipboard (Zipper xs_cb p_cb@(Point ph_cb j_cb))) -> modify_ _
      { term = state.term # atZipperCursor zc # fst # \wrap_zc -> wrap_zc \xs_zc -> xs_cb # atPoint p_cb # (_ $ xs_zc)
      , cursor = InjectPoint $ Point (ph1_zc `nestPaths` ph_cb) j_cb
      }
handleUserAction (StartDrag p) = do
  modify_ _ { cursor = InjectPoint p }
handleUserAction (StartDrag_double p1 _p1') = do
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
                [ HH.ClassName "Cursor", HH.ClassName "active" ]
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
          pointHandleProps (Point ph (NT.wrap (NT.unwrap i)))
        ] # Array.fold
      )
      [ HH.text a ]
  ]
renderTerm'_helper f ph i (Group _n xs) =
  [ -- a Group's left paren is a Point handle for the Point right _before_ it
    [ renderPointHandle (Point ph (NT.wrap (NT.unwrap i))) [ HH.ClassName "Paren", HH.ClassName "OpenParen" ] "(" ]
  , f (ph `List.snoc` i) xs
  , -- a Group's right paren is a Point handle for the Point right _after_ it
    [ renderPointHandle (Point ph (NT.wrap (NT.unwrap i + 1))) [ HH.ClassName "Paren", HH.ClassName "CloseParen" ] ")" ]
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
                ( [ [ HH.ClassName "Cursor", HH.ClassName "StartSpanHandle" ]
                  , if sh == StartSpanHandle then [ HH.ClassName "active" ] else []
                  ] # Array.fold
                )
                "["
            ]
          else if j2 == j' then
            [ renderPointHandle (Point ph j')
                ( [ [ HH.ClassName "Cursor", HH.ClassName "EndSpanHandle" ]
                  , if sh == EndSpanHandle then [ HH.ClassName "active" ] else []
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
                  ( [ [ HH.ClassName "Cursor", HH.ClassName "OuterStartZipperHandle" ]
                    , if zh == OuterStartZipperHandle then [ HH.ClassName "active" ] else []
                    ] # Array.fold
                  )
                  "{"
              ]
            else if j2 == j' then
              [ renderPointHandle (Point ph j')
                  ( [ [ HH.ClassName "Cursor", HH.ClassName "OuterEndZipperHandle" ]
                    , if zh == OuterEndZipperHandle then [ HH.ClassName "active" ] else []
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
                  ( [ [ HH.ClassName "Cursor", HH.ClassName "OuterStartZipperHandle" ]
                    , if zh == OuterStartZipperHandle then [ HH.ClassName "active" ] else []
                    ] # Array.fold
                  )
                  "{"
              ]
            else if j2 == j' then
              [ renderPointHandle (Point ph j')
                  ( [ [ HH.ClassName "Cursor", HH.ClassName "OuterEndZipperHandle" ]
                    , if zh == OuterEndZipperHandle then [ HH.ClassName "active" ] else []
                    ] # Array.fold
                  )
                  "}"
              ]
            else if j1' == j' && j2' == j' then
              [ renderPointHandle (Point ph j')
                  ( [ [ HH.ClassName "Cursor", HH.ClassName "InnerStartZipperHandle" ]
                    , if zh == InnerStartZipperHandle then [ HH.ClassName "active" ] else []
                    ] # Array.fold
                  )
                  "X"
              ]
            else if j1' == j' then
              [ renderPointHandle (Point ph j')
                  ( [ [ HH.ClassName "Cursor", HH.ClassName "InnerStartZipperHandle" ]
                    , if zh == InnerStartZipperHandle then [ HH.ClassName "active" ] else []
                    ] # Array.fold
                  )
                  "⟨"
              ]
            else if j2' == j' then
              [ renderPointHandle (Point ph j')
                  ( [ [ HH.ClassName "Cursor", HH.ClassName "InnerEndZipperHandle" ]
                    , if zh == InnerEndZipperHandle then [ HH.ClassName "active" ] else []
                    ] # Array.fold
                  )
                  "⟩"
              ]
            else
              [ renderPointHandle (Point ph j') [ HH.ClassName "Space" ] "•" ]
        )
        ( \(_i /\ _zos') j ->
            [ renderPointHandle (Point ph j) [ HH.ClassName "Space" ] "•" ]
        )
        ( \(j1 /\ j2) j' ->
            if j1 == j' && j2 == j' then
              [ renderPointHandle (Point ph j')
                  ( [ [ HH.ClassName "Cursor", HH.ClassName "InnerStartZipperHandle" ]
                    , if zh == InnerStartZipperHandle then [ HH.ClassName "active" ] else []
                    ] # Array.fold
                  )
                  "X"
              ]
            else if j1 == j' then
              [ renderPointHandle (Point ph j')
                  ( [ [ HH.ClassName "Cursor", HH.ClassName "InnerStartZipperHandle" ]
                    , if zh == InnerStartZipperHandle then [ HH.ClassName "active" ] else []
                    ] # Array.fold
                  )
                  "⟨"
              ]
            else if j2 == j' then
              [ renderPointHandle (Point ph j')
                  ( [ [ HH.ClassName "Cursor", HH.ClassName "InnerEndZipperHandle" ]
                    , if zh == InnerEndZipperHandle then [ HH.ClassName "active" ] else []
                    ] # Array.fold
                  )
                  "⟩"
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
          pointHandleProps_double (Point ph (NT.wrap (NT.unwrap i))) (Point ph (NT.wrap (NT.unwrap i + 1)))
        ] # Array.fold
      )
      [ HH.text a ]
  ]
renderTerm' ph i (Group _n xs) =
  [ -- a Group's left paren is a Point handle for the Point right _before_ it
    [ HH.div
        ( [ [ HP.classes [ HH.ClassName "Point", HH.ClassName "Paren", HH.ClassName "OpenParen" ] ]
          , pointHandleProps_double (Point ph (NT.wrap (NT.unwrap i))) (Point ph (NT.wrap (NT.unwrap i + 1)))
          ]
            # Array.fold
        )
        [ HH.text "(" ]
    ]
  , renderTerm (ph `List.snoc` i) xs
  , -- a Group's right paren is a Point handle for the Point right _after_ it
    [ HH.div
        ( [ [ HP.classes [ HH.ClassName "Point", HH.ClassName "Paren", HH.ClassName "CloseParen" ] ]
          , pointHandleProps_double (Point ph (NT.wrap (NT.unwrap i))) (Point ph (NT.wrap (NT.unwrap i + 1)))
          ]
            # Array.fold
        )
        [ HH.text ")" ]
    ]
  ] # Array.fold

renderPointHandle :: Point -> Array HH.ClassName -> String -> HTML
renderPointHandle p cns s =
  HH.div
    ( [ [ HP.classes ([ HH.ClassName "Point" ] <> cns) ]
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
