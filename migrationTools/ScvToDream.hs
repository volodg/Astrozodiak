{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- Dream Book
-- https://docs.google.com/spreadsheets/d/1QEVJvwrRpGvuvVh2XAKaNQWem5HwYh7q9D7Cs9-2lFE/edit#gid=0

import Control.Monad.Trans.Resource --(MonadResource, allocate)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class --(liftIO)
import qualified Data.Conduit.Binary as CB
import qualified Data.Text as T
import qualified Data.Map as DM
import Data.CSV.Conduit
import Data.Conduit
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql
import Data.Pool (destroyAllResources)
import Model
import Database.Persist
import qualified Database.Esqueleto as E
import GHC.Int

connStr = "host=localhost dbname=Astrozodiak user=vladimirgorbenko password=H4d3yl8x port=5432"

openConn :: IO ConnectionPool
openConn = putStrLn "start" >> (runStdoutLoggingT $ createPostgresqlPool connStr 10)

closeConn :: ConnectionPool -> IO ()
closeConn pool = do
    putStrLn "stop"
    destroyAllResources pool

putToDBForever :: (MonadResource m) => ConnectionPool -> Maybe Int64 -> Sink (Int64, Dream) m (Maybe Int64)
putToDBForever pool lastID = do
    dreamWithID <- await
    case dreamWithID of
        Just (dreamID, dream) -> do
            liftIO $ flip runSqlPool pool $ repsert (toSqlKey dreamID) dream
            putToDBForever pool $ Just dreamID
        Nothing -> return lastID

sink :: MonadResource m => Sink (Int64, Dream) m () -- consumes a stream of Strings, no result
sink = do
    (_releaseKey, pool) <- allocate openConn closeConn
    res <- putToDBForever pool Nothing
    liftIO $ case res of
        Just key -> flip runSqlPool pool $ deleteWhere [DreamId >. toSqlKey key]
        Nothing -> return ()
    return ()

csvRawToDream :: MapRow T.Text -> Int64 -> Maybe (Int64, Dream)
csvRawToDream raw fieldID = do
    context <- rawLookup "context"
    rawType <- rawLookup "type"
    word    <- rawLookup "word"
    return (fieldID, Dream word context (T.pack "ru") rawType) where
        rawLookup key = DM.lookup (T.pack key) raw

toDream :: MonadResource m => Int64 -> Conduit (MapRow T.Text) m (Int64, Dream)
toDream lineNum = do
    lineOpt <- await
    case lineOpt of
        Just line -> do
            case csvRawToDream line lineNum of
                Just el -> yield el
                Nothing -> liftIO $ putStrLn $ "can not parse line: " ++ show line
            toDream (lineNum + 1)
        _ -> return ()

csvToDB = runResourceT $
  CB.sourceFile "DreamBook-ru.csv" $=
  intoCSV defCSVSettings $=
  toDream 1 $$
  sink

main = csvToDB
