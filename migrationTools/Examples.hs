{-# LANGUAGE DataKinds #-}

module Main where

-- Dream Book
-- https://docs.google.com/spreadsheets/d/1QEVJvwrRpGvuvVh2XAKaNQWem5HwYh7q9D7Cs9-2lFE/edit#gid=0

import Control.Monad.Cont
import Data.Conduit
import Control.Monad.Trans.Resource
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString as B
import Data.ByteString.Char8 as BC hiding (writeFile, readFile)
import Control.Monad.Trans.State
import Model
import qualified Data.Text as T
import Data.List.Split
import Debug.Trace
import qualified Data.Map as DM

-- import Data.Conduit.Binary
import Data.CSV.Conduit

sink :: MonadResource m => Sink (Int, Dream) m () -- consumes a stream of Strings, no result
sink = CL.mapM_ $ \x -> do
    (_releaseKey, _resource) <- allocate (return ()) (\_ -> return ())
    liftIO $ Prelude.putStrLn $ show x
    return ()

csvRawToDream :: MapRow T.Text -> Int -> Maybe (Int, Dream)
csvRawToDream raw fieldID = do
    context <- rawLookup "context"
    rawType <- rawLookup "type"
    word    <- rawLookup "word"
    return (fieldID, Dream word context (T.pack "ru") rawType) where
        rawLookup key = DM.lookup (T.pack key) raw

toDream :: MonadResource m => Int -> Conduit (MapRow T.Text) m (Int, Dream)
toDream lineNum = do
    lineOpt <- await
    case lineOpt of
        Just line -> do
            case csvRawToDream line lineNum of
                Just el -> yield $ el
                Nothing -> liftIO $ Prelude.putStrLn $ "can not parse line: " ++ show line
            toDream (lineNum + 1)
        _ -> return ()

test = runResourceT $
  CB.sourceFile "DreamBook-ru.csv" $=
  intoCSV defCSVSettings $=
  (toDream 1) $$
  sink

main = test
