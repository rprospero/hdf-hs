module Lib
    ( someFunc
    ) where

import Data.Binary (decodeFileOrFail)
import Data.Binary.Get (ByteOffset)
import HDF.Types

loadFile :: String -> IO (Either (ByteOffset, String) HDF)
loadFile = decodeFileOrFail

someFunc :: IO ()
someFunc = do
  loadFile "c:/Users/auv61894/Documents/Code/LARMOR00016814.nxs" >>= print
  loadFile "c:/Users/auv61894/Documents/Code/Detector_Mask.xml" >>= print
  return ()
