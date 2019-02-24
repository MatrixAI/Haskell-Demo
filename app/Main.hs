module Main where

import qualified FFI as F
import           Lib (someFunc)

main :: IO ()
main = do
  time <- F.getTime
  let pi = F.getPi
  let negPi = F.getNegPi
  let sum = F.negAdd 1 2
  putStrLn $ show time
  putStrLn $ show pi
  putStrLn $ show negPi
  putStrLn $ show sum
  someFunc
