module Main where

import Lib

main :: IO ()
main = do
    name <- getLine
    let x = 1 + y
        y = 7
    putStrLn $ "Hallo " ++ show x ++ " " ++ name

f :: IO Integer
f = return $ 1 + 2 
