{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Api (getApiR) where

import Data.Text (Text, pack, append)
import Yesod (ToJSON, Value, toJSON, (.=), object, returnJson, lookupGetParam, liftIO)
import Prelude hiding (head, init, readFile, tail, writeFile) -- , last
import Foundation (Handler)
import Handler.Common (StatusCode(Success,NoCmd,UnsupportedCmd,UnhandledException))
import Data.Typeable (typeOf)
import Control.Exception (SomeException(SomeException), evaluate, handle)
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

firstCommand :: IO Value
firstCommand = evaluate $ toJSON (last []::[String])

secondCommand :: IO Value
secondCommand = return $ toJSON ("2 command"::String)

handlerByCmd :: Text -> Maybe (IO Value)
handlerByCmd "1" = Just firstCommand
handlerByCmd "2" = Just secondCommand
handlerByCmd _ = Nothing

exceptionHandler :: SomeException -> IO Value
exceptionHandler (SomeException e) = return $ toJSON $ Status (fromEnum UnhandledException) errorDesc Nothing where
    errorDesc = pack $ (show (typeOf e) ++ ":" ++ show e)

wrapStatusOK :: Value -> Value
wrapStatusOK val = toJSON $ Status (fromEnum Success) "ok" (Just val)

handlerByParam :: Maybe Text -> IO Value
handlerByParam (Just cmd) = do
    case handlerByCmd cmd of
        Just handler -> do
            handle exceptionHandler (wrapStatusOK <$> handler)
        Nothing -> return $ toJSON $ Status (fromEnum UnsupportedCmd) msg Nothing where
            msg = foldl append (pack "") [pack "command: ", cmd, pack " is unsupported"]

handlerByParam Nothing = return $ toJSON $ Status (fromEnum NoCmd) "parameter \"r={some_command}\" is required" Nothing

getApiR :: Handler Value
getApiR = do
    lookupGetParam "r" >>= liftIO . handlerByParam >>= returnJson
