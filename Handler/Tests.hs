{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Handler.Tests (getTests, getTest) where

--import Database.Esqueleto.Internal.Sql (unsafeSqlBinOp)
import Foundation (Handler)
import Text.Read (readMaybe)
--import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack) --, pack, append)
import Prelude hiding (head, init, last, readFile, tail, writeFile)
import Yesod (Value, ToJSON, (.=), object, toJSON, returnJson, lookupGetParam, runDB)
import qualified Database.Esqueleto as E
import           GHC.Int
import qualified Data.Map as Map

import Handler.Common (Status(..), StatusCode(InvalidParameters))

import Model

instance ToJSON (E.Value TestId, E.Value Text) where
    toJSON (E.Value testId, E.Value name) = object
        [ "id"   .= testId
        , "name" .= name
        ]

sqlSelectTests :: Handler [(E.Value TestId, E.Value Text)]
sqlSelectTests =
    runDB $ E.select $
        E.from $ \test -> do
            E.orderBy [E.asc (test E.^. TestId)]
            return (test E.^. TestId, test E.^. TestName)

getTests :: Handler Value
getTests = sqlSelectTests >>= returnJson . toJSON

data RecordWithKey a = RecordWithKey Int64 a

instance Eq (RecordWithKey a) where
    (==) (RecordWithKey k1 _) (RecordWithKey k2 _) = k1 == k2

instance Ord (RecordWithKey a) where
    (<)  (RecordWithKey k1 _) (RecordWithKey k2 _) = k1 <  k2
    (<=) (RecordWithKey k1 _) (RecordWithKey k2 _) = k1 <= k2

data QuestNameWithAnswers = QuestNameWithAnswers Text [Answer]

instance ToJSON QuestNameWithAnswers where
    toJSON (QuestNameWithAnswers name answers) = object
        [ "name"   .= name
        , "answer" .= answers
        ]

-- TODO test and make faster
questAnswerRawTransform :: [(E.Entity Quest, E.Entity Answer)] -> [QuestNameWithAnswers]
questAnswerRawTransform raws = 
    Map.foldWithKey toArr [] dict where
        addEl dictAcc (E.Entity k1 v1, E.Entity _ v2) = Map.alter (appendAnswer v2) (RecordWithKey (E.fromSqlKey k1) v1) dictAcc where
            appendAnswer answer answers = Just $ case answers of
                Just els -> answer : els
                Nothing  -> [answer]
        dict = foldl addEl Map.empty raws
        toArr (RecordWithKey _ (Quest txt _)) val acc = QuestNameWithAnswers txt (reverse val) : acc

sqlQuests :: Int64 -> Handler [QuestNameWithAnswers]
sqlQuests testId = do
    entities <- runDB $ E.select $
                    E.from $ \(quest, answer) -> do
                        E.where_ $ quest E.^. QuestOwner E.==. E.valkey testId E.&&. answer E.^. AnswerOwner E.==. quest E.^. QuestId
                        E.orderBy [E.asc (quest E.^. QuestId)]
                        return (quest, answer)

    return $ questAnswerRawTransform entities

sqlResults :: Int64 -> Handler [TestResult]
sqlResults testId = do
    entities <- runDB $ E.select $
                E.from $ \result -> do
                    E.where_ $ result E.^. TestResultOwner E.==. E.valkey testId
                    E.orderBy [E.asc (result E.^. TestResultId)]
                    return result
    return $ map (\(E.Entity _ v) -> v) entities

getTest :: Handler Value
getTest = do
    testIdParamOpt <- lookupGetParam "id"
    case testIdParamOpt of
        Nothing -> return $ toJSON $ Status (fromEnum InvalidParameters) "'id' (test identifier) argument not specified" Nothing
        Just testIdParam ->
            case readMaybe $ unpack testIdParam :: Maybe Int64 of
                Nothing -> return $ toJSON $ Status (fromEnum InvalidParameters) "'id' (test identifier) is not a number" Nothing
                Just testId -> do
                    -- TODO make parallel DB requests
                    quests  <- sqlQuests  testId
                    results <- sqlResults testId
                    return $ object [ "quest" .= quests, "result" .= results ]
