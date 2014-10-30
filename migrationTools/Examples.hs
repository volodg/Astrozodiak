module Examples where

import System.IO.Unsafe

helper i = unsafePerformIO $ print i >> return i

main = do
    let one = helper 1
        two = helper 2
    print $ one + two
