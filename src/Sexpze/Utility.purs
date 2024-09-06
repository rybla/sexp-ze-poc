module Sexpze.Utility where

import Prelude

import Partial.Unsafe (unsafeCrashWith)

todo :: forall a b. String -> a -> b
todo msg _ = unsafeCrashWith ("TODO: " <> msg)
