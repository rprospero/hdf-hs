module Lib
    ( someFunc
    ) where

import Data.Binary (decodeFile)
import HDF.Types

loadFile :: IO SuperBlock
loadFile = decodeFile "c:/Users/auv61894/Documents/Code/LARMOR00016814.nxs"

someFunc :: IO ()
someFunc = do
  loadFile >>= print
  return ()
