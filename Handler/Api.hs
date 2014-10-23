{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.Api (getApiR) where

import Data.Text (Text, pack, unpack, append)
import Yesod
import Prelude
-- import Import
import Foundation
import Yesod.Core.Json
import Control.Applicative ((<$>), (<*>))

data Status = Status
    { status  :: Int
    , message :: Text
    , content :: Maybe Value
    }

instance ToJSON Status where
    toJSON (Status status message content) = object
        [ "status"  .= status
        , "message" .= message
        , "data"    .= content
        ]

data Person = Person
    { name :: Text
    , age  :: Int
    }

instance ToJSON Person where
    toJSON (Person name age) = object
        [ "name" .= name
        , "age"  .= age
        ]

firstCommand :: IO Value
firstCommand = return $ toJSON ("1 command"::String)

secondCommand :: IO Value
secondCommand = return $ toJSON ("2 command"::String)

wrapStatusOK :: Value -> Value
wrapStatusOK val = toJSON $ Status 0 "ok" (Just val)

hamdlerByCmd :: Text -> Maybe (IO Value)
hamdlerByCmd "1" = Just firstCommand
hamdlerByCmd "2" = Just firstCommand
hamdlerByCmd _ = Nothing

routeByParam :: Maybe Text -> IO Value
routeByParam (Just cmd) = do
	case hamdlerByCmd cmd of
		Just handler -> do
			wrapStatusOK <$> handler
		Nothing -> return $ toJSON $ Status 2 msg Nothing where
			msg = foldl append (pack "") [pack "command: ", cmd, pack " is unsupported"]

routeByParam Nothing = return $ toJSON $ Status 1 "parameter \"r={some_command}\" is required" Nothing

getApiR :: Handler Value
getApiR = do
	param  <- lookupGetParam "r"
	result <- liftIO $ routeByParam param
	returnJson $ result