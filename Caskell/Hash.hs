{-# LANGUAGE OverloadedStrings #-}
{-|
   module to provide hashing

   inspired by Unison hash
|-}

module Caskell.Hash
(
    Hash,
    Hashable,
    typeID,
    uniqueBytes,

    binaryToHash,
    getHash,
    primitiveBytes
) where


import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Crypto.Hash as CH

import Data.Array
import Data.Binary
import Data.Maybe
import Data.Ratio
import Data.Word
import Pair
import FastString
import Unique

import Caskell.Bytes

type TypeID = Word32
type Hash = CH.Digest CH.SHA3_512

binaryToHash :: [BS.ByteString] -> Hash
binaryToHash = CH.hashFinalize . CH.hashUpdates CH.hashInit

getHash :: (Hashable a) => a -> Hash
getHash = binaryToHash . uniqueBytes

class Hashable a where
    typeID :: a -> TypeID
    uniqueBytes :: a -> [BS.ByteString]

typeID' :: Hashable a => a -> [BS.ByteString]
typeID' = toBytes . typeID 

primitiveBytes :: (Hashable a, BinarySerializable a) => a -> [BS.ByteString]
primitiveBytes x = bytes where
    tb = typeID' x
    bts = toBytes x
    lb = toBytes (sum $ map (BS.length) bts)
    bytes = tb ++ lb ++ bts

-- primitives
instance Hashable Word where
    typeID = const 0x00000000
    uniqueBytes = primitiveBytes

instance Hashable Word8 where
    typeID = const 0x00000001
    uniqueBytes = primitiveBytes

instance Hashable Word16 where
    typeID = const 0x00000002
    uniqueBytes = primitiveBytes

instance Hashable Word32 where
    typeID = const 0x00000003
    uniqueBytes = primitiveBytes

instance Hashable Int where
    typeID = const 0x00000004
    uniqueBytes = primitiveBytes

instance Hashable Integer where
    typeID = const 0x00000005
    uniqueBytes = primitiveBytes

instance Hashable Char where
    typeID = const 0x00000006
    uniqueBytes = primitiveBytes

instance Hashable Float where
    typeID = const 0x00000007
    uniqueBytes = primitiveBytes

instance Hashable Double where
    typeID = const 0x00000008
    uniqueBytes = primitiveBytes

instance Hashable Bool where
    typeID = const 0x00000009
    uniqueBytes = primitiveBytes

-- not-so-primitives
instance Hashable BS.ByteString where
    typeID = const 0x0000000A
    uniqueBytes = primitiveBytes

instance Hashable BSL.ByteString where
    typeID = const 0x0000000B
    uniqueBytes = primitiveBytes

instance (Hashable a) => Hashable [a] where
    typeID = const 0x0000000C
    uniqueBytes x = bytes where
        tb = typeID' x
        bts = concatMap (uniqueBytes) x
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts

instance (Hashable a, Hashable b)
  => Hashable (a, b) where
    typeID = const 0x0000000D
    uniqueBytes (x, y) = bytes where
        tb = typeID' (x, y)
        bts1 = uniqueBytes x
        bts2 = uniqueBytes y
        len1 = sum $ map (BS.length) bts1
        len2 = sum $ map (BS.length) bts2
        lb = toBytes (len1 + len2)
        bytes = tb ++ lb ++ bts1 ++ bts2

-- the rest
instance Hashable a => Hashable (Ratio a) where
    typeID = const 0x0000000E
    uniqueBytes x = bytes where
        tb = typeID' x
        bts1 = uniqueBytes $ numerator x
        bts2 = uniqueBytes $ denominator x
        len1 = sum $ map (BS.length) bts1
        len2 = sum $ map (BS.length) bts2
        lb = toBytes (len1 + len2)
        bytes = tb ++ lb ++ bts1 ++ bts2

instance Hashable FastString where
    typeID = const 0x0000000F
    uniqueBytes = primitiveBytes

instance Hashable a => Hashable (Maybe a) where
    typeID (Just _) = 0x00000010
    typeID Nothing  = 0x00000011
    uniqueBytes x = bytes where
        tb = typeID' x
        bts = case x of
          Just y -> uniqueBytes y
          Nothing -> []
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts

instance (Ix i, Hashable i, Hashable v) => Hashable (Array i v) where
    typeID = const 0x00000012
    uniqueBytes x = bytes where
        tb = typeID' x
        bts = concatMap (\(i,v) -> uniqueBytes i ++ uniqueBytes v) $ assocs x
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts

instance (Hashable a) => Hashable (Pair a) where
    typeID = const 0x00000013
    uniqueBytes x = bytes where
        tb = typeID' x
        bts = uniqueBytes (pFst x) ++ uniqueBytes (pSnd x)
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts

{-|
-- UNIQUES ARE NOT STABLE ACROSS REBUILDS
-- https://hackage.haskell.org/package/ghc-8.6.4/docs/src/Unique.html#nonDetCmpUnique
instance Hashable Unique where
    typeID = const 0x00000014
    uniqueBytes x = bytes where
        tb = typeID' x
        bts = uniqueBytes $ Unique.getKey x
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts
|-}
