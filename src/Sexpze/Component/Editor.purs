module Sexpze.Component.Editor where

import Web.UIEvent.KeyboardEvent (KeyboardEvent)

data Query a = KeyboardEvent_Query KeyboardEvent a

data Output = Output

