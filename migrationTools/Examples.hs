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

sink :: MonadResource m => Sink (Int, Dream) m () -- consumes a stream of Strings, no result
sink = CL.mapM_ $ \x -> do
    (_releaseKey, _resource) <- allocate (return ()) (\_ -> return ())
    liftIO $ Prelude.putStrLn $ show x
    return ()

strLineToDream :: B.ByteString -> Int -> (Int, Dream)
strLineToDream line fieldID = (fieldID, Dream word context (T.pack "ru") dreamType) where
    lineStr = BC.unpack line
    [word, context, dreamType, _] = Prelude.map T.pack $ splitOn "," lineStr
--    word = 

numLines :: MonadResource m => Int -> Conduit B.ByteString m (Int, Dream)
numLines lineNum = do
    lineOpt <- await
    case lineOpt of
        Just line ->
            let next = numLines (lineNum + 1) in
            if (lineNum /= 0)
                then do
                    yield $ strLineToDream line lineNum
                    next
                else
                    next
        _ -> return ()

main = do
    runResourceT $ CB.sourceFile "DreamBook-ru.csv" $$ CB.lines =$ (numLines 0) =$ sink
