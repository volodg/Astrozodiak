{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.Api (getApiR) where

import Data.Text (Text, pack, append)
import Yesod
import Prelude
import Foundation
import Handler.Common
import Control.Applicative ((<$>))

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

data Person = Person Text Int
    -- { name :: Text
    -- , age  :: Int
    -- }

instance ToJSON Person where
    toJSON (Person name age) = object
        [ "name" .= name
        , "age"  .= age
        ]

firstCommand :: IO Value
firstCommand = return $ toJSON ("1 command"::String)

secondCommand :: IO Value
secondCommand = return $ toJSON ("2 command"::String)

hamdlerByCmd :: Text -> Maybe (IO Value)
hamdlerByCmd "1" = Just firstCommand
hamdlerByCmd "2" = Just secondCommand
hamdlerByCmd _ = Nothing

routeByParam :: Maybe Text -> IO Value
routeByParam (Just cmd) = do
    case hamdlerByCmd cmd of
        Just handler -> do
            let wrapStatusOK val = toJSON $ Status (fromEnum Success) "ok" (Just val) in
                wrapStatusOK <$> handler
        Nothing -> return $ toJSON $ Status (fromEnum UnsupportedCmd) msg Nothing where
            msg = foldl append (pack "") [pack "command: ", cmd, pack " is unsupported"]

routeByParam Nothing = return $ toJSON $ Status (fromEnum NoCmd) "parameter \"r={some_command}\" is required" Nothing

getApiR :: Handler Value
getApiR = do
    param  <- lookupGetParam "r"
    result <- liftIO $ routeByParam param
    returnJson $ result