{-# LANGUAGE FlexibleInstances #-}
{-|
   module to provide hashing

   inspired by Unison hash
|-}

module Caskell.Hash
(
    Hash,
    TypeID,
    TypeIDAble,
    typeID,
    typeIDBytes,
    typeName,
    UniquelySerializable,
    uniqueBytes,
    Hashable,
    get_hash,
    HashOrIDBytes(..),

    binary_to_hash,
    manual_unique_bytes,
    primitive_bytes,
    uniqueBytesFromMaybe
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

binary_to_hash :: Bytes -> Hash
binary_to_hash = CH.hashFinalize . CH.hashUpdates CH.hashInit

class TypeIDAble a where
    typeID :: a -> TypeID
    typeName :: a -> String
    typeName = const ""

class (TypeIDAble a) => UniquelySerializable a where
    uniqueBytes :: a -> Bytes

class Hashable a where
    get_hash :: a -> Hash

-- this datatype is used to either propagate a hash or calculate
-- a hash from unique bytes
data HashOrIDBytes = H Hash
                   | B
                   { bTypeID :: TypeID
                   , bBytes :: Bytes
                   } deriving (Eq, Show)

typeIDBytes :: TypeIDAble a => a -> Bytes
typeIDBytes = toBytes . typeID 

manual_unique_bytes :: TypeID -> Bytes -> Bytes
manual_unique_bytes td bts = bytes where
    tb = toBytes td
    len = fromIntegral (sum $ map (BS.length) bts) :: Int
    lenb = toBytes len
    bytes = tb ++ lenb ++ bts

primitive_bytes :: (TypeIDAble a, BinarySerializable a) => a -> Bytes
primitive_bytes x = bytes where
    td = typeID x
    bts = toBytes x
    bytes = manual_unique_bytes td bts

hash_from_unique_bytes :: (UniquelySerializable a) => a -> Hash
hash_from_unique_bytes = binary_to_hash . uniqueBytes

uniqueBytesFromMaybe :: UniquelySerializable a => Maybe a -> Bytes
uniqueBytesFromMaybe (Just x) = uniqueBytes x
uniqueBytesFromMaybe Nothing  = []

-- primitives
instance TypeIDAble Word where
    typeID = const 0x00000000

instance UniquelySerializable Word where
    uniqueBytes = primitive_bytes

instance Hashable Word where
    get_hash = hash_from_unique_bytes

instance TypeIDAble Word8 where
    typeID = const 0x00000001

instance UniquelySerializable Word8 where
    uniqueBytes = primitive_bytes

instance Hashable Word8 where
    get_hash = hash_from_unique_bytes

instance TypeIDAble Word16 where
    typeID = const 0x00000002

instance UniquelySerializable Word16 where
    uniqueBytes = primitive_bytes

instance Hashable Word16 where
    get_hash = hash_from_unique_bytes

instance TypeIDAble Word32 where
    typeID = const 0x00000003

instance UniquelySerializable Word32 where
    uniqueBytes = primitive_bytes

instance Hashable Word32 where
    get_hash = hash_from_unique_bytes

instance TypeIDAble Int where
    typeID = const 0x00000004

instance UniquelySerializable Int where
    uniqueBytes = primitive_bytes

instance Hashable Int where
    get_hash = hash_from_unique_bytes

instance TypeIDAble Integer where
    typeID = const 0x00000005

instance UniquelySerializable Integer where
    uniqueBytes = primitive_bytes

instance Hashable Integer where
    get_hash = hash_from_unique_bytes

instance TypeIDAble Char where
    typeID = const 0x00000006

instance UniquelySerializable Char where
    uniqueBytes = primitive_bytes

instance Hashable Char where
    get_hash = hash_from_unique_bytes

instance TypeIDAble Float where
    typeID = const 0x00000007

instance UniquelySerializable Float where
    uniqueBytes = primitive_bytes

instance Hashable Float where
    get_hash = hash_from_unique_bytes

instance TypeIDAble Double where
    typeID = const 0x00000008

instance UniquelySerializable Double where
    uniqueBytes = primitive_bytes

instance Hashable Double where
    get_hash = hash_from_unique_bytes

instance TypeIDAble Bool where
    typeID = const 0x00000009

instance UniquelySerializable Bool where
    uniqueBytes = primitive_bytes

instance Hashable Bool where
    get_hash = hash_from_unique_bytes

-- not-so-primitives
instance TypeIDAble BS.ByteString where
    typeID = const 0x0000000A

instance UniquelySerializable BS.ByteString where
    uniqueBytes = primitive_bytes

instance Hashable BS.ByteString where
    get_hash = hash_from_unique_bytes

instance TypeIDAble BSL.ByteString where
    typeID = const 0x0000000B

instance UniquelySerializable BSL.ByteString where
    uniqueBytes = primitive_bytes

instance Hashable BSL.ByteString where
    get_hash = hash_from_unique_bytes

instance TypeIDAble [a] where
    typeID = const 0x0000000C

instance (UniquelySerializable a) => UniquelySerializable [a] where
    uniqueBytes x = bytes where
        tb = typeIDBytes x
        bts = concatMap (uniqueBytes) x
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (fromIntegral (BS.length $ head lb) :: Word8)) ++ lb ++ bts

instance (UniquelySerializable a) => Hashable [a] where
    get_hash = hash_from_unique_bytes

-- the rest
instance TypeIDAble (Ratio a) where
    typeID = const 0x0000000E

instance UniquelySerializable a => UniquelySerializable (Ratio a) where
    uniqueBytes x = bytes where
        tb = typeIDBytes x
        bts1 = uniqueBytes $ numerator x
        bts2 = uniqueBytes $ denominator x
        len1 = sum $ map (BS.length) bts1
        len2 = sum $ map (BS.length) bts2
        lb = toBytes (len1 + len2)
        bytes = tb ++ (toBytes (fromIntegral (BS.length $ head lb) :: Word8)) ++ lb ++ bts1 ++ bts2

instance UniquelySerializable a => Hashable (Ratio a) where
    get_hash = hash_from_unique_bytes

instance TypeIDAble FastString where
    typeID = const 0x0000000F

instance UniquelySerializable FastString where
    uniqueBytes = primitive_bytes

instance Hashable FastString where
    get_hash = hash_from_unique_bytes

instance TypeIDAble (Maybe a) where
    typeID (Just _) = 0x00000010
    typeID Nothing  = 0x00000011

instance UniquelySerializable a => UniquelySerializable (Maybe a) where
    uniqueBytes x = bytes where
        tb = typeIDBytes x
        bts = case x of
          Just y -> uniqueBytes y
          Nothing -> []
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (fromIntegral (BS.length $ head lb) :: Word8)) ++ lb ++ bts

instance UniquelySerializable a => Hashable (Maybe a) where
    get_hash = hash_from_unique_bytes

instance TypeIDAble (Array i v) where
    typeID = const 0x00000012

instance (Ix i, UniquelySerializable i, UniquelySerializable v) => UniquelySerializable (Array i v) where
    uniqueBytes x = bytes where
        tb = typeIDBytes x
        bts = concatMap (\(i,v) -> uniqueBytes i ++ uniqueBytes v) $ assocs x
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (fromIntegral (BS.length $ head lb) :: Word8)) ++ lb ++ bts

instance (Ix i, UniquelySerializable i, UniquelySerializable v) => Hashable (Array i v) where
    get_hash = hash_from_unique_bytes

instance TypeIDAble (Pair a) where
    typeID = const 0x00000013

instance (UniquelySerializable a) => UniquelySerializable (Pair a) where
    uniqueBytes x = bytes where
        tb = typeIDBytes x
        bts = uniqueBytes (pFst x) ++ uniqueBytes (pSnd x)
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (fromIntegral (BS.length $ head lb) :: Word8)) ++ lb ++ bts

instance UniquelySerializable a => Hashable (Pair a) where
    get_hash = hash_from_unique_bytes

{-|
-- UNIQUES ARE NOT STABLE ACROSS REBUILDS
-- https://hackage.haskell.org/package/ghc-8.6.4/docs/src/Unique.html#nonDetCmpUnique
instance UniquelySerializable Unique where
    typeID = const 0x00000014
    uniqueBytes x = bytes where
        tb = typeIDBytes x
        bts = uniqueBytes $ Unique.getKey x
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (fromIntegral (BS.length $ head lb) :: Word8)) ++ lb ++ bts
|-}


instance TypeIDAble (a, b) where
    typeID = const 0x00000100

instance (UniquelySerializable a, UniquelySerializable b)
  => UniquelySerializable (a, b) where
    uniqueBytes (x, y) = bytes where
        tb = typeIDBytes (x, y)
        bts1 = uniqueBytes x
        bts2 = uniqueBytes y
        len1 = sum $ map (BS.length) bts1
        len2 = sum $ map (BS.length) bts2
        lb = toBytes (len1 + len2)
        bytes = tb ++ (toBytes (fromIntegral (BS.length $ head lb) :: Word8)) ++ lb ++ bts1 ++ bts2

instance (UniquelySerializable a, UniquelySerializable b)
  => Hashable (a, b) where
    get_hash = hash_from_unique_bytes

instance TypeIDAble (a, b, c) where
    typeID = const 0x00000101

instance (UniquelySerializable a, UniquelySerializable b, UniquelySerializable c)
  => UniquelySerializable (a, b, c) where
    uniqueBytes (x, y, z) = bytes where
        tb = typeIDBytes (x, y, z)
        bts1 = uniqueBytes x
        bts2 = uniqueBytes y
        bts3 = uniqueBytes z
        len1 = sum $ map (BS.length) bts1
        len2 = sum $ map (BS.length) bts2
        len3 = sum $ map (BS.length) bts3
        lb = toBytes (len1 + len2 + len3)
        bytes = tb ++ (toBytes (fromIntegral (BS.length $ head lb) :: Word8)) ++ lb ++ bts1 ++ bts2 ++ bts3

instance (UniquelySerializable a, UniquelySerializable b, UniquelySerializable c)
  => Hashable (a, b, c) where
    get_hash = hash_from_unique_bytes

instance TypeIDAble (a, b, c, d) where
    typeID = const 0x00000102

instance (UniquelySerializable a, UniquelySerializable b, UniquelySerializable c, UniquelySerializable d)
  => UniquelySerializable (a, b, c, d) where
    uniqueBytes (x, y, z, u) = bytes where
        tb = typeIDBytes (x, y, z, u)
        bts1 = uniqueBytes x
        bts2 = uniqueBytes y
        bts3 = uniqueBytes z
        bts4 = uniqueBytes u
        len1 = sum $ map (BS.length) bts1
        len2 = sum $ map (BS.length) bts2
        len3 = sum $ map (BS.length) bts3
        len4 = sum $ map (BS.length) bts4
        lb = toBytes (len1 + len2 + len3 + len4)
        bytes = tb ++ (toBytes (fromIntegral (BS.length $ head lb) :: Word8)) ++ lb ++ bts1 ++ bts2 ++ bts3 ++ bts4

instance (UniquelySerializable a, UniquelySerializable b, UniquelySerializable c, UniquelySerializable d)
  => Hashable (a, b, c, d) where
    get_hash = hash_from_unique_bytes

-- Hashes
instance Hashable Hash where
    get_hash = id

instance Hashable HashOrIDBytes where
    get_hash (H h) = h
    get_hash (B id bts) = get_hash $ manual_unique_bytes id bts

