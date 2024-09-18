module Sexpze.Data.Sexp.Cursor.Pretty where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data T = B (Array T) | L String

derive instance Generic T _

instance Show T where
  show x = genericShow x

-- prettySpanWithCursor :: ZipperCursor \/ SpanCursor -> ZipperHandle -> Path -> Term -> Array T
-- prettySpanWithCursor = todo ""