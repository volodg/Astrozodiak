module Handler.Common (StatusCode(..)) where

import Prelude

data StatusCode = Success | NoCmd | UnsupportedCmd deriving (Enum)