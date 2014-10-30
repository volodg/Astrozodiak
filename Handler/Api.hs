{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Api (getApiR) where

import Handler.Common (Status(..), StatusCode(Success,NoCmd,UnsupportedCmd,UnhandledException))
import Handler.DreamBook (dreamBookAutocomplete, getDreamBook)

import Yesod.Core.Types (HandlerT(HandlerT), unHandlerT)
import Data.Text (Text, pack, append)
import Yesod (Value, toJSON, returnJson, lookupGetParam, liftIO)
import Prelude hiding (head, init, readFile, tail, writeFile)
import Foundation (Handler)
import Data.Typeable (typeOf)
import Control.Exception (SomeException(SomeException), evaluate, handle)
import Control.Applicative ((<$>))

firstCommand :: Handler Value
firstCommand = liftIO $ evaluate $ toJSON (last []::[String])

secondCommand :: Handler Value
secondCommand = return $ toJSON ("2 command"::String)

handlerByCmd :: Text -> Maybe (Handler Value)
handlerByCmd "parse/dreambookautocomplete" = Just dreamBookAutocomplete
handlerByCmd "parse/getdream" = Just getDreamBook
handlerByCmd "1" = Just firstCommand
handlerByCmd "2" = Just secondCommand
handlerByCmd _ = Nothing

exceptionHandler :: SomeException -> IO Value
exceptionHandler (SomeException e) = return $ toJSON $ Status (fromEnum UnhandledException) errorDesc Nothing where
    errorDesc = pack (show (typeOf e) ++ ":" ++ show e)

wrapStatusOK :: Value -> Value
wrapStatusOK val = toJSON $ Status (fromEnum Success) "ok" (Just val)

handlerByParam :: Maybe Text -> Handler Value
handlerByParam (Just cmd) =
    case handlerByCmd cmd of
        Just handler ->
            HandlerT (handle exceptionHandler . unHandlerT (wrapStatusOK <$> handler))
        Nothing -> return $ toJSON $ Status (fromEnum UnsupportedCmd) msg Nothing where
            msg = foldl append (pack "") [pack "command: ", cmd, pack " is unsupported"]

handlerByParam Nothing = return $ toJSON $ Status (fromEnum NoCmd) "parameter \"r={some_command}\" is required" Nothing

getApiR :: Handler Value
getApiR = lookupGetParam "r" >>= handlerByParam >>= returnJson
