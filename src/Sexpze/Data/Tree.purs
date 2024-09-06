module Sexpze.Data.Tree where

import Prelude

data Tree a = Tree a (Array (Tree a))

