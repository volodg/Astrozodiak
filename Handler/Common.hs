module Handler.Common (StatusCode(..), Status(..), toJSON) where

import Prelude hiding (head, init, last, readFile, tail, writeFile)
import Yesod (ToJSON, Value, toJSON, (.=), object)
import Data.Text (Text)

data Status = Status Int Text (Maybe Value)
    -- { status  :: Int
    -- , message :: Text
    -- , content :: Maybe Value
    -- }

instance ToJSON Status where
    toJSON (Status status message content) = object
        [ "status"  .= status
        , "message" .= message
        , "data"    .= content
        ]

data StatusCode =
 Success            | -- 0
 NoCmd              | -- 1
 UnsupportedCmd     | -- 2
 UnhandledException | -- 3
 InvalidParameters    -- 4
 deriving (Enum)