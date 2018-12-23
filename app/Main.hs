module Main where

import           Lib

main :: IO ()
main = do
  let x = 1 + y
      y = 7
  putStrLn "Hello from Haskell-Docker."

f :: IO Integer
f = return $ 1 + 2
