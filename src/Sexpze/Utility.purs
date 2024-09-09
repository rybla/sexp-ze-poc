module Sexpze.Utility where

import Prelude

import Partial.Unsafe (unsafeCrashWith)

bug :: forall a. String -> String -> a
bug src msg = unsafeCrashWith $ "[bug at " <> src <> "] " <> msg

assert :: forall a. String -> Boolean -> (Unit -> a) -> a
assert label b k = if b then k unit else bug "assert" $ label

todo :: forall a b. String -> a -> b
todo msg _ = unsafeCrashWith ("[todo] " <> msg)
