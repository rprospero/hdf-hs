{-# LANGUAGE DeriveGeneric #-}
module HDF.Types where

import Control.Monad (replicateM)
import Data.Binary
import GHC.Generics

pad :: Int -> Get ()
pad n = do
  _ <- replicateM n getWord8
  return ()

data Pad = Pad deriving (Eq)

instance Show Pad where
  show _ = ""

instance Binary Pad where
  get = do
    getWord8
    return Pad
  put _ = putWord8 0

data OAddresses a = OAddresses a a a a
  deriving (Show, Eq)

instance Binary a => Binary (OAddresses a) where
  get = OAddresses <$> get <*> get <*> get <*> get
  put (OAddresses a b c d) = put a >> put b >> put c >> put d

data OldAddresses = O1 (OAddresses Word)
  | O2 (OAddresses Word16)
  | O4 (OAddresses Word32)
  | O8 (OAddresses Word64)
  deriving (Show, Eq)

getOldAddresses :: Word8 -> Get OldAddresses
getOldAddresses 1 = O1 <$> get
getOldAddresses 2 = O2 <$> get
getOldAddresses 4 = O4 <$> get
getOldAddresses 8 = O8 <$> get

putOldAddresses (O1 x) = put x
putOldAddresses (O2 x) = put x
putOldAddresses (O4 x) = put x
putOldAddresses (O8 x) = put x

data OldSuperBlockBody = OldSuperBlockBody Pad Pad Pad Pad Word8 Word8 Pad Word16 Word16 Word32
  deriving (Show, Eq, Generic)

instance Binary OldSuperBlockBody

data NewSuperBlockBody = NewSuperBlockBody
  deriving (Show, Eq, Generic)

instance Binary NewSuperBlockBody where

data SuperBlock =
  V0 OldSuperBlockBody OldAddresses
  | V1 OldSuperBlockBody Word16 Word16 OldAddresses
  | V2 NewSuperBlockBody
  deriving (Show, Eq)

superblockSignature :: [Word8]
superblockSignature = [137, 72, 68, 70, 13, 10, 26, 10]

instance Binary SuperBlock where
  get= do
    replicateM 8 (get :: Get Pad)
    version <- getWord8
    case version of
      0 -> do
        body <- get
        let (OldSuperBlockBody _ _ _ _ s _ _ _ _ _) = body
        addr <- getOldAddresses s
        return $ V0 body addr
      1 -> do
        body@(OldSuperBlockBody _ _ _ _ s _ _ _ _ _) <- get
        a <- get
        b <- get
        addr <- getOldAddresses s
        return $ V1 body a b addr
      _ -> V2 <$> get
  put (V0 x addr) = do
    put superblockSignature
    put (0 :: Word8)
    put x
    putOldAddresses addr
  put (V1 x a b addr) = do
    put superblockSignature
    put (1 :: Word8)
    put x >> put a >> put b >> putOldAddresses addr
  put (V2 x) = do
    put superblockSignature
    put (1 :: Word8)
    put x
