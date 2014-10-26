{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Handler.DreamBook (dreamBookAutocomplete) where

import Database.Esqueleto.Internal.Sql (unsafeSqlBinOp)
import Foundation (Handler)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack, append)
import Prelude hiding (head, init, last, readFile, tail, writeFile)
import Yesod (Value, toJSON, returnJson, lookupGetParam, runDB)
import qualified Database.Esqueleto as E

import Model

ilike :: forall a b c. E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value b) -> E.SqlExpr (E.Value c)
ilike = unsafeSqlBinOp "ILIKE"

sqlSelect :: Maybe Text -> Int -> Handler [Text]
sqlSelect Nothing _ = return []
sqlSelect (Just term) limit = do
    entities <- runDB $ E.selectDistinct $
                E.from $ \dream -> do
                E.where_ (dream E.^. DreamWord `ilike` (E.val $ append term (pack "%")))
                E.orderBy [E.asc (dream E.^. DreamWord)]
                E.limit $ fromIntegral limit
                return $ dream E.^. DreamWord
    return $ map (\(E.Value v) -> v) entities

parseLimitParam :: Maybe Text -> Int -> Int
parseLimitParam val defValue = fromMaybe defValue (val >>= readMaybe . unpack :: Maybe Int)

dreamBookAutocomplete :: Handler Value
dreamBookAutocomplete = do
    term  <- lookupGetParam "term"
    limit <- lookupGetParam "limit"
    terms <- sqlSelect term (parseLimitParam limit 5)
    returnJson $ toJSON terms
