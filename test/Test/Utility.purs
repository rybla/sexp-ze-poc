module Test.Utility where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)
import Test.Spec (Spec, it)
import Test.Spec.Assertions (fail)

shouldEqual :: forall m a. Applicative m => Eq a => MonadThrow Error m => Show a => { actual :: a, expected :: a } -> m Unit
shouldEqual { expected, actual } = unless (expected == actual) $ fail $ "expected:\n\n    " <> show expected <> "\n\n  actual:\n\n    " <> show actual <> "\n"

it_shouldEqual :: forall a. Eq a => Show a => String -> { actual :: a, expected :: a } -> Spec Unit
it_shouldEqual label = it label <<< shouldEqual