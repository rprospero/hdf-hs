{-# LANGUAGE DeriveGeneric #-}
module HDF.Types where

import Control.Monad (replicateM)
import Data.Binary
import Data.Binary.Get (skip)
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

data Address = A1 Word8 | A2 Word16 | A4 Word32 | A8 Word64
  deriving (Show, Eq)

instance Binary Address where
  get = fail "Attempting to load address of unknown size"
  put (A1 x) = put x
  put (A2 x) = put x
  put (A4 x) = put x
  put (A8 x) = put x

addressInt :: Address -> Int
addressInt (A1 x) = fromIntegral x
addressInt (A2 x) = fromIntegral x
addressInt (A4 x) = fromIntegral x
addressInt (A8 x) = fromIntegral x

getAddress :: Word8 -> Get Address
getAddress 1 = A1 <$> get
getAddress 2 = A2 <$> get
getAddress 4 = A4 <$> get
getAddress 8 = A8 <$> get
getAddress x = fail $ show x ++ " is an unexpected by count for an address"

data Offset = O1 Word8 | O2 Word16 | O4 Word32 | O8 Word64
  deriving (Show, Eq)

getOffset :: Word8 -> Get Offset
getOffset 1 = O1 <$> get
getOffset 2 = O2 <$> get
getOffset 4 = O4 <$> get
getOffset 8 = O8 <$> get
getOffset x = fail $ show x ++ " is an unexpected by count for an offset"

instance Binary Offset where
  get = fail "Attempting to load Offset of unknown size"
  put (O1 x) = put x
  put (O2 x) = put x
  put (O4 x) = put x
  put (O8 x) = put x


data LazyLoaded x = LazyLoaded Address
  deriving (Show, Eq)

loadLazy :: Binary x => LazyLoaded x -> Get x
loadLazy (LazyLoaded x) = skip (addressInt x) >> get

data OldAddresses = OldAddresses Word8 Address Address Address Address
  deriving (Show, Eq)

getOldAddresses :: Word8 -> Get OldAddresses
getOldAddresses size = OldAddresses size <$> getAddress size <*> getAddress size <*> getAddress size <*> getAddress size

putOldAddresses :: OldAddresses -> Put
putOldAddresses (OldAddresses _ a b c d) = put a >> put b >> put c >> put d

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
    signature <- replicateM 8 get
    if signature /= superblockSignature
      then fail "Bad Format Signature.  I don't think that this is an HDF file."
      else return ()
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
      2 -> V2 <$> get
      _ -> fail $ "Unknown superblock type: " ++ show version
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
