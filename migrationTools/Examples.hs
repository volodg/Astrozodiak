module Main where

import Control.Monad.Cont

ex2 = do
    a <- return 1
    b <- cont (\fred -> fred 10)
    return $ a+b

test2 = runCont ex2 show

ex3 = do
    a <- return 1
    b <- cont (\fred -> "escape")
    return $ a+b

test3 = runCont ex3 show

ex4 = do
    a <- return 1
    b <- cont (\fred -> fred 10 ++ fred 20)
    return $ a+b

test4 = runCont ex4 show

ex7 = do
    a <- return 1
    b <- cont (\fred -> [10,20] >>= fred)
    return $ a+b

test7 = runCont ex7 (\x -> [x])

i x = cont (\fred -> x >>= fred)

run m = runCont m return

test9 = run $ do
    a <- i [1, 2]
    b <- i [10,20]
    return $ a+b

main = print test9
