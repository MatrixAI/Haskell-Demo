module Main where

import qualified FFI                           as F
import           Lib                            ( someFunc )

main :: IO ()
main = do
  time <- F.getTime
  let foreignPi  = F.getPi
  let negPi      = F.getNegPi
  let foreignSum = F.negAdd 1 2
  print time
  print foreignPi
  print negPi
  print foreignSum
  someFunc
