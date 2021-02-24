{-# LANGUAGE OverloadedStrings #-}
{-|
    module to convert things into a ByteString

    inspired by Unison hash
|-}

module Caskell.Bytes
(
    BinarySerializable,
    Bytes,
    toBytes,
    bytesLength
) where

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Crypto.Hash as CH

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Binary
import Data.Ratio
import FastString

type Bytes = [BS.ByteString]

class BinarySerializable a where
    toBytes :: a -> Bytes

bytesLength :: (Num b) => Bytes -> b
bytesLength = fromIntegral . sum . map (BS.length)

-- basics
instance BinarySerializable BS.ByteString where
    toBytes x = [x]

instance BinarySerializable BSL.ByteString where
    toBytes = BSL.toChunks

instance BinarySerializable (CH.Digest a) where
    toBytes d = [BA.convert d]

instance BinarySerializable a => BinarySerializable [a] where
    toBytes = concat . map (toBytes)

instance (BinarySerializable a, BinarySerializable b) => BinarySerializable (a, b) where
    toBytes (x, y) = toBytes x ++ toBytes y

-- etc
instance BinarySerializable a => BinarySerializable (Set.Set a) where
    toBytes = toBytes . Set.toList

instance (BinarySerializable k, BinarySerializable v) => BinarySerializable (Map.Map k v) where
    toBytes = toBytes . Map.toList

instance BinarySerializable Word where
    toBytes = toBytes . encode

instance BinarySerializable Word8 where
    toBytes = toBytes . encode

instance BinarySerializable Word16 where
    toBytes = toBytes . encode

instance BinarySerializable Word32 where
    toBytes = toBytes . encode

instance BinarySerializable Int where
    toBytes = toBytes . encode

instance BinarySerializable Integer where
    toBytes = toBytes . encode

instance BinarySerializable Char where
    toBytes = toBytes . encode

instance BinarySerializable Float where
    toBytes = toBytes . encode

instance BinarySerializable Double where
    toBytes = toBytes . encode

instance BinarySerializable a => BinarySerializable (Ratio a) where
    toBytes x = toBytes (numerator x, denominator x)

instance BinarySerializable Bool where
    toBytes = toBytes . encode

instance BinarySerializable () where
    toBytes _ = []

instance BinarySerializable FastString where
    toBytes = toBytes . FastString.unpackFS
