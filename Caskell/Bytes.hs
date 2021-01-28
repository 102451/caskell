{-# LANGUAGE OverloadedStrings #-}
{-|
    module to convert things into a ByteString

    inspired by Unison hash
|-}

module Caskell.Bytes
(
    BinarySerializable,
    toBytes
) where

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Binary

class BinarySerializable a where
    toBytes :: a -> [BS.ByteString]

-- basics
instance BinarySerializable BS.ByteString where
    toBytes x = [x]

instance BinarySerializable BSL.ByteString where
    toBytes = BSL.toChunks

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

instance BinarySerializable Bool where
    toBytes = toBytes . encode

instance BinarySerializable () where
    toBytes _ = []
