module Sexpze.Utility where

import Prelude

import Control.Apply (lift2)
import Control.Plus (empty)
import Data.Array as Array
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Foldable (class Foldable, foldr)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafeCrashWith)

bug :: forall a. String -> String -> a
bug src msg = unsafeCrashWith $ "[bug at " <> src <> "] " <> msg

assert :: forall a. String -> Boolean -> (Unit -> a) -> a
assert label b k = if b then k unit else bug "assert" $ label

todo :: forall a b. String -> a -> b
todo msg _ = unsafeCrashWith ("[todo] " <> msg)

unimplemented :: forall a b. String -> a -> b
unimplemented msg _ = unsafeCrashWith ("[unimplemented] " <> msg)

allEqual :: forall f a. Foldable f => Eq a => f a -> Boolean
allEqual =
  fst <<<
    foldr
      (\x (b /\ mb_y) -> if not b then false /\ mb_y else mb_y # maybe (true /\ pure x) (\y -> (x == y) /\ mb_y))
      (true /\ empty)

