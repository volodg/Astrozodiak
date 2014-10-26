{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Handler.DreamBook (dreamBookAutocomplete) where

import Foundation (Handler)
import Data.Text (Text, pack, append)
import Prelude hiding (head, init, last, readFile, tail, writeFile)
import Yesod (Value, toJSON, returnJson, lookupGetParam, runDB)
-- (ToJSON, (.=), object, returnJson, liftIO)
import qualified Database.Esqueleto as E

-- TODO use https://www.fpcomplete.com/school/starting-with-haskell/libraries-and-frameworks/persistent-db
-- 
import Model

sqlSelect :: Maybe Text -> Handler [Text]
sqlSelect Nothing = return []
sqlSelect (Just term) = do
    entities <- runDB $ E.select $
                E.from $ \dream -> do
                E.where_ (dream E.^. DreamWord `E.like` (E.val $ append term (pack "%")))
                return $ dream E.^. DreamWord
    return $ map (\(E.Value v) -> v) entities

dreamBookAutocomplete :: Handler Value
dreamBookAutocomplete = do
    term <- lookupGetParam "term"
    terms <- sqlSelect term
    returnJson $ toJSON terms
    ---returnJson $ toJSON (terms::[String])