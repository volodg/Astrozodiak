{-# LANGUAGE CPP            #-}
{-# LANGUAGE PackageImports #-}
import           "Astrozodiak" Application              (getApplicationDev)
import           Control.Concurrent       (forkIO, threadDelay)
import           Network.Wai.Handler.Warp (defaultSettings, runSettings,
                                           setPort)
import           System.Directory         (doesFileExist, removeFile)
import           System.Exit              (exitSuccess)

#ifndef mingw32_HOST_OS
import           System.Posix.Signals     (Handler (Catch), installHandler,
                                           sigINT)
#endif

main :: IO ()
main = do
#ifndef mingw32_HOST_OS
    _ <- installHandler sigINT (Catch $ return ()) Nothing
#endif

    putStrLn "Starting devel application"
    (port, app) <- getApplicationDev
    forkIO $ runSettings (setPort port defaultSettings) app
    loop

loop :: IO ()
loop = do
  threadDelay 100000
  e <- doesFileExist "yesod-devel/devel-terminate"
  if e then terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = exitSuccess
