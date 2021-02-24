{-# LANGUAGE OverloadedStrings #-}
{-|
   module to provide hashing

   inspired by Unison hash
|-}

module Caskell.Hash
(
    Hash,
    TypeID,
    TypeIDAble,
    Hashable,
    typeID,
    typeName,
    uniqueBytes,

    binary_to_hash,
    get_hash,
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

binary_to_hash :: [BS.ByteString] -> Hash
binary_to_hash = CH.hashFinalize . CH.hashUpdates CH.hashInit

get_hash :: (Hashable a) => a -> Hash
get_hash = binary_to_hash . uniqueBytes

class TypeIDAble a where
    typeID :: a -> TypeID
    typeName :: a -> String
    typeName = const ""

class (TypeIDAble a) => Hashable a where
    uniqueBytes :: a -> [BS.ByteString]

typeID' :: Hashable a => a -> [BS.ByteString]
typeID' = toBytes . typeID 

primitive_bytes :: (Hashable a, BinarySerializable a) => a -> [BS.ByteString]
primitive_bytes x = bytes where
    tb = typeID' x
    bts = toBytes x
    lb = toBytes (sum $ map (BS.length) bts)
    bytes = tb ++ (toBytes (fromIntegral (BS.length $ head lb) :: Word8)) ++ lb ++ bts

uniqueBytesFromMaybe :: Hashable a => Maybe a -> Bytes
uniqueBytesFromMaybe (Just x) = uniqueBytes x
uniqueBytesFromMaybe Nothing  = []

-- primitives
instance TypeIDAble Word where
    typeID = const 0x00000000

instance Hashable Word where
    uniqueBytes = primitive_bytes

instance TypeIDAble Word8 where
    typeID = const 0x00000001

instance Hashable Word8 where
    uniqueBytes = primitive_bytes

instance TypeIDAble Word16 where
    typeID = const 0x00000002

instance Hashable Word16 where
    uniqueBytes = primitive_bytes

instance TypeIDAble Word32 where
    typeID = const 0x00000003

instance Hashable Word32 where
    uniqueBytes = primitive_bytes

instance TypeIDAble Int where
    typeID = const 0x00000004

instance Hashable Int where
    uniqueBytes = primitive_bytes

instance TypeIDAble Integer where
    typeID = const 0x00000005

instance Hashable Integer where
    uniqueBytes = primitive_bytes

instance TypeIDAble Char where
    typeID = const 0x00000006

instance Hashable Char where
    uniqueBytes = primitive_bytes

instance TypeIDAble Float where
    typeID = const 0x00000007

instance Hashable Float where
    uniqueBytes = primitive_bytes

instance TypeIDAble Double where
    typeID = const 0x00000008

instance Hashable Double where
    uniqueBytes = primitive_bytes

instance TypeIDAble Bool where
    typeID = const 0x00000009

instance Hashable Bool where
    uniqueBytes = primitive_bytes

-- not-so-primitives
instance TypeIDAble BS.ByteString where
    typeID = const 0x0000000A

instance Hashable BS.ByteString where
    uniqueBytes = primitive_bytes

instance TypeIDAble BSL.ByteString where
    typeID = const 0x0000000B

instance Hashable BSL.ByteString where
    uniqueBytes = primitive_bytes

instance TypeIDAble [a] where
    typeID = const 0x0000000C

instance (Hashable a) => Hashable [a] where
    uniqueBytes x = bytes where
        tb = typeID' x
        bts = concatMap (uniqueBytes) x
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (fromIntegral (BS.length $ head lb) :: Word8)) ++ lb ++ bts

-- the rest
instance TypeIDAble (Ratio a) where
    typeID = const 0x0000000E

instance Hashable a => Hashable (Ratio a) where
    uniqueBytes x = bytes where
        tb = typeID' x
        bts1 = uniqueBytes $ numerator x
        bts2 = uniqueBytes $ denominator x
        len1 = sum $ map (BS.length) bts1
        len2 = sum $ map (BS.length) bts2
        lb = toBytes (len1 + len2)
        bytes = tb ++ (toBytes (fromIntegral (BS.length $ head lb) :: Word8)) ++ lb ++ bts1 ++ bts2

instance TypeIDAble FastString where
    typeID = const 0x0000000F

instance Hashable FastString where
    uniqueBytes = primitive_bytes

instance TypeIDAble (Maybe a) where
    typeID (Just _) = 0x00000010
    typeID Nothing  = 0x00000011

instance Hashable a => Hashable (Maybe a) where
    uniqueBytes x = bytes where
        tb = typeID' x
        bts = case x of
          Just y -> uniqueBytes y
          Nothing -> []
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (fromIntegral (BS.length $ head lb) :: Word8)) ++ lb ++ bts

instance TypeIDAble (Array i v) where
    typeID = const 0x00000012

instance (Ix i, Hashable i, Hashable v) => Hashable (Array i v) where
    uniqueBytes x = bytes where
        tb = typeID' x
        bts = concatMap (\(i,v) -> uniqueBytes i ++ uniqueBytes v) $ assocs x
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (fromIntegral (BS.length $ head lb) :: Word8)) ++ lb ++ bts

instance TypeIDAble (Pair a) where
    typeID = const 0x00000013

instance (Hashable a) => Hashable (Pair a) where
    uniqueBytes x = bytes where
        tb = typeID' x
        bts = uniqueBytes (pFst x) ++ uniqueBytes (pSnd x)
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (fromIntegral (BS.length $ head lb) :: Word8)) ++ lb ++ bts

{-|
-- UNIQUES ARE NOT STABLE ACROSS REBUILDS
-- https://hackage.haskell.org/package/ghc-8.6.4/docs/src/Unique.html#nonDetCmpUnique
instance Hashable Unique where
    typeID = const 0x00000014
    uniqueBytes x = bytes where
        tb = typeID' x
        bts = uniqueBytes $ Unique.getKey x
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ (toBytes (fromIntegral (BS.length $ head lb) :: Word8)) ++ lb ++ bts
|-}


instance TypeIDAble (a, b) where
    typeID = const 0x00000100

instance (Hashable a, Hashable b)
  => Hashable (a, b) where
    uniqueBytes (x, y) = bytes where
        tb = typeID' (x, y)
        bts1 = uniqueBytes x
        bts2 = uniqueBytes y
        len1 = sum $ map (BS.length) bts1
        len2 = sum $ map (BS.length) bts2
        lb = toBytes (len1 + len2)
        bytes = tb ++ (toBytes (fromIntegral (BS.length $ head lb) :: Word8)) ++ lb ++ bts1 ++ bts2

instance TypeIDAble (a, b, c) where
    typeID = const 0x00000101

instance (Hashable a, Hashable b, Hashable c)
  => Hashable (a, b, c) where
    uniqueBytes (x, y, z) = bytes where
        tb = typeID' (x, y, z)
        bts1 = uniqueBytes x
        bts2 = uniqueBytes y
        bts3 = uniqueBytes z
        len1 = sum $ map (BS.length) bts1
        len2 = sum $ map (BS.length) bts2
        len3 = sum $ map (BS.length) bts3
        lb = toBytes (len1 + len2 + len3)
        bytes = tb ++ (toBytes (fromIntegral (BS.length $ head lb) :: Word8)) ++ lb ++ bts1 ++ bts2 ++ bts3

instance TypeIDAble (a, b, c, d) where
    typeID = const 0x00000102

instance (Hashable a, Hashable b, Hashable c, Hashable d)
  => Hashable (a, b, c, d) where
    uniqueBytes (x, y, z, u) = bytes where
        tb = typeID' (x, y, z, u)
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
