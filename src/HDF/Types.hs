{-# LANGUAGE DeriveGeneric #-}
module HDF.Types where

import Control.Monad (replicateM)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import GHC.Generics

newtype LWord16 = LWord16 Word16
  deriving (Show, Eq)
newtype LWord32 = LWord32 Word32
  deriving (Show, Eq)
newtype LWord64 = LWord64 Word64
  deriving (Show, Eq)

instance Binary LWord16 where
  get = LWord16 <$> getWord16le
  put (LWord16 x) = put x

instance Binary LWord32 where
  get = LWord32 <$> getWord32le
  put (LWord32 x) = put x

instance Binary LWord64 where
  get = LWord64 <$> getWord64le
  put (LWord64 x) = put x

-- Note to self: Use a custom word type to fix the endian issues

pad :: Int -> Get ()
pad n = do
  _ <- replicateM n getWord8
  return ()

data Pad = Pad deriving (Eq)

instance Show Pad where
  show _ = ""

instance Binary Pad where
  get = do
    skip 1
    return Pad
  put _ = putWord8 0

data Address = A1 Word8 | A2 LWord16 | A4 LWord32 | A8 LWord64
  deriving (Show, Eq)

instance Binary Address where
  get = fail "Attempting to load address of unknown size"
  put (A1 x) = put x
  put (A2 x) = put x
  put (A4 x) = put x
  put (A8 x) = put x

addressInt :: Address -> Int
addressInt (A1 x) = fromIntegral x
addressInt (A2 (LWord16 x)) = fromIntegral x
addressInt (A4 (LWord32 x)) = fromIntegral x
addressInt (A8 (LWord64 x)) = fromIntegral x

getAddress :: Word8 -> Get Address
getAddress 1 = A1 <$> get
getAddress 2 = A2 <$> get
getAddress 4 = A4 <$> get
getAddress 8 = A8 <$> get
getAddress x = fail $ show x ++ " is an unexpected by count for an address"

data Offset = O1 Word8 | O2 LWord16 | O4 LWord32 | O8 LWord64
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

type AddressAndOffset = (Word8, Word8)

address :: AddressAndOffset -> Word8
address = fst
offset :: AddressAndOffset -> Word8
offset = snd

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

data OldSuperBlockBody = OldSuperBlockBody Pad Pad Pad Pad AddressAndOffset Pad LWord16 LWord16 LWord32
  deriving (Show, Eq, Generic)

data NewSuperBlockBody = NewSuperBlockBody
  deriving (Show, Eq, Generic)

instance Binary NewSuperBlockBody where

data SuperBlock =
  V0 OldSuperBlockBody OldAddresses
  | V1 OldSuperBlockBody LWord16 LWord16 OldAddresses
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
        let (OldSuperBlockBody _ _ _ _ s _ _ _ _) = body
        addr <- getOldAddresses $ address s
        return $ V0 body addr
      1 -> do
        body@(OldSuperBlockBody _ _ _ _ s _ _ _ _) <- get
        a <- get
        b <- get
        addr <- getOldAddresses $ address s
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

aao :: SuperBlock -> AddressAndOffset
aao (V0 (OldSuperBlockBody _ _ _ _ s _ _ _ _) _) = s
aao (V1 (OldSuperBlockBody _ _ _ _ s _ _ _ _) _ _ _) = s
aao (V2 _) = (0, 0) -- FIXME

---- Symbol Entry Table

data SymbolEntryTable = SymbolEntryTable Offset Address LWord32
  deriving (Show, Eq, Generic)

getSymbolEntryTable :: AddressAndOffset -> Get SymbolEntryTable
getSymbolEntryTable x = do
  SymbolEntryTable <$> getOffset (offset x) <*> getAddress (address x) <*> get

putSymbolEntryTable :: SymbolEntryTable -> Put
putSymbolEntryTable (SymbolEntryTable off add cache) = do
  put off
  put add
  put cache

-- HDF File

data HDF = HDF SuperBlock SymbolEntryTable
  deriving (Show, Eq)

instance Binary HDF where
  get = do
    block <- get
    HDF block <$> getSymbolEntryTable (aao block)
  put (HDF block table) = put block >> putSymbolEntryTable table
