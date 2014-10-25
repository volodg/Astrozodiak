module Handler.Common (StatusCode(..)) where

import Prelude

data StatusCode =
 Success        |   -- 0
 NoCmd          |   -- 1
 UnsupportedCmd |   -- 2
 UnhandledException -- 3
 deriving (Enum)