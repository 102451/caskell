
module Caskell.PrimOpHash
(
    uniqueBytes
) where

import Data.Word
import qualified Data.ByteString as BS
import Caskell.Bytes
import Caskell.Hash
import qualified PrimOp
import qualified CmmType

typeID' :: Hashable a => a -> [BS.ByteString]
typeID' = toBytes . typeID 

-- PrimOp.PrimOp
instance Hashable PrimOp.PrimOp where
    typeID x = case x of
      PrimOp.CharGtOp -> 0x00015000
      PrimOp.CharGeOp -> 0x00015001
      PrimOp.CharEqOp -> 0x00015002
      PrimOp.CharNeOp -> 0x00015003
      PrimOp.CharLtOp -> 0x00015004
      PrimOp.CharLeOp -> 0x00015005
      PrimOp.OrdOp -> 0x00015006
      PrimOp.IntAddOp -> 0x00015007
      PrimOp.IntSubOp -> 0x00015008
      PrimOp.IntMulOp -> 0x00015009
      PrimOp.IntMulMayOfloOp -> 0x0001500a
      PrimOp.IntQuotOp -> 0x0001500b
      PrimOp.IntRemOp -> 0x0001500c
      PrimOp.IntQuotRemOp -> 0x0001500d
      PrimOp.AndIOp -> 0x0001500e
      PrimOp.OrIOp -> 0x0001500f
      PrimOp.XorIOp -> 0x00015010
      PrimOp.NotIOp -> 0x00015011
      PrimOp.IntNegOp -> 0x00015012
      PrimOp.IntAddCOp -> 0x00015013
      PrimOp.IntSubCOp -> 0x00015014
      PrimOp.IntGtOp -> 0x00015015
      PrimOp.IntGeOp -> 0x00015016
      PrimOp.IntEqOp -> 0x00015017
      PrimOp.IntNeOp -> 0x00015018
      PrimOp.IntLtOp -> 0x00015019
      PrimOp.IntLeOp -> 0x0001501a
      PrimOp.ChrOp -> 0x0001501b
      PrimOp.Int2WordOp -> 0x0001501c
      PrimOp.Int2FloatOp -> 0x0001501d
      PrimOp.Int2DoubleOp -> 0x0001501e
      PrimOp.Word2FloatOp -> 0x0001501f
      PrimOp.Word2DoubleOp -> 0x00015020
      PrimOp.ISllOp -> 0x00015021
      PrimOp.ISraOp -> 0x00015022
      PrimOp.ISrlOp -> 0x00015023
      PrimOp.Int8Extend -> 0x00015024
      PrimOp.Int8Narrow -> 0x00015025
      PrimOp.Int8NegOp -> 0x00015026
      PrimOp.Int8AddOp -> 0x00015027
      PrimOp.Int8SubOp -> 0x00015028
      PrimOp.Int8MulOp -> 0x00015029
      PrimOp.Int8QuotOp -> 0x0001502a
      PrimOp.Int8RemOp -> 0x0001502b
      PrimOp.Int8QuotRemOp -> 0x0001502c
      PrimOp.Int8EqOp -> 0x0001502d
      PrimOp.Int8GeOp -> 0x0001502e
      PrimOp.Int8GtOp -> 0x0001502f
      PrimOp.Int8LeOp -> 0x00015030
      PrimOp.Int8LtOp -> 0x00015031
      PrimOp.Int8NeOp -> 0x00015032
      PrimOp.Word8Extend -> 0x00015033
      PrimOp.Word8Narrow -> 0x00015034
      PrimOp.Word8NotOp -> 0x00015035
      PrimOp.Word8AddOp -> 0x00015036
      PrimOp.Word8SubOp -> 0x00015037
      PrimOp.Word8MulOp -> 0x00015038
      PrimOp.Word8QuotOp -> 0x00015039
      PrimOp.Word8RemOp -> 0x0001503a
      PrimOp.Word8QuotRemOp -> 0x0001503b
      PrimOp.Word8EqOp -> 0x0001503c
      PrimOp.Word8GeOp -> 0x0001503d
      PrimOp.Word8GtOp -> 0x0001503e
      PrimOp.Word8LeOp -> 0x0001503f
      PrimOp.Word8LtOp -> 0x00015040
      PrimOp.Word8NeOp -> 0x00015041
      PrimOp.Int16Extend -> 0x00015042
      PrimOp.Int16Narrow -> 0x00015043
      PrimOp.Int16NegOp -> 0x00015044
      PrimOp.Int16AddOp -> 0x00015045
      PrimOp.Int16SubOp -> 0x00015046
      PrimOp.Int16MulOp -> 0x00015047
      PrimOp.Int16QuotOp -> 0x00015048
      PrimOp.Int16RemOp -> 0x00015049
      PrimOp.Int16QuotRemOp -> 0x0001504a
      PrimOp.Int16EqOp -> 0x0001504b
      PrimOp.Int16GeOp -> 0x0001504c
      PrimOp.Int16GtOp -> 0x0001504d
      PrimOp.Int16LeOp -> 0x0001504e
      PrimOp.Int16LtOp -> 0x0001504f
      PrimOp.Int16NeOp -> 0x00015050
      PrimOp.Word16Extend -> 0x00015051
      PrimOp.Word16Narrow -> 0x00015052
      PrimOp.Word16NotOp -> 0x00015053
      PrimOp.Word16AddOp -> 0x00015054
      PrimOp.Word16SubOp -> 0x00015055
      PrimOp.Word16MulOp -> 0x00015056
      PrimOp.Word16QuotOp -> 0x00015057
      PrimOp.Word16RemOp -> 0x00015058
      PrimOp.Word16QuotRemOp -> 0x00015059
      PrimOp.Word16EqOp -> 0x0001505a
      PrimOp.Word16GeOp -> 0x0001505b
      PrimOp.Word16GtOp -> 0x0001505c
      PrimOp.Word16LeOp -> 0x0001505d
      PrimOp.Word16LtOp -> 0x0001505e
      PrimOp.Word16NeOp -> 0x0001505f
      PrimOp.WordAddOp -> 0x00015060
      PrimOp.WordAddCOp -> 0x00015061
      PrimOp.WordSubCOp -> 0x00015062
      PrimOp.WordAdd2Op -> 0x00015063
      PrimOp.WordSubOp -> 0x00015064
      PrimOp.WordMulOp -> 0x00015065
      PrimOp.WordMul2Op -> 0x00015066
      PrimOp.WordQuotOp -> 0x00015067
      PrimOp.WordRemOp -> 0x00015068
      PrimOp.WordQuotRemOp -> 0x00015069
      PrimOp.WordQuotRem2Op -> 0x0001506a
      PrimOp.AndOp -> 0x0001506b
      PrimOp.OrOp -> 0x0001506c
      PrimOp.XorOp -> 0x0001506d
      PrimOp.NotOp -> 0x0001506e
      PrimOp.SllOp -> 0x0001506f
      PrimOp.SrlOp -> 0x00015070
      PrimOp.Word2IntOp -> 0x00015071
      PrimOp.WordGtOp -> 0x00015072
      PrimOp.WordGeOp -> 0x00015073
      PrimOp.WordEqOp -> 0x00015074
      PrimOp.WordNeOp -> 0x00015075
      PrimOp.WordLtOp -> 0x00015076
      PrimOp.WordLeOp -> 0x00015077
      PrimOp.PopCnt8Op -> 0x00015078
      PrimOp.PopCnt16Op -> 0x00015079
      PrimOp.PopCnt32Op -> 0x0001507a
      PrimOp.PopCnt64Op -> 0x0001507b
      PrimOp.PopCntOp -> 0x0001507c
      PrimOp.Pdep8Op -> 0x0001507d
      PrimOp.Pdep16Op -> 0x0001507e
      PrimOp.Pdep32Op -> 0x0001507f
      PrimOp.Pdep64Op -> 0x00015080
      PrimOp.PdepOp -> 0x00015081
      PrimOp.Pext8Op -> 0x00015082
      PrimOp.Pext16Op -> 0x00015083
      PrimOp.Pext32Op -> 0x00015084
      PrimOp.Pext64Op -> 0x00015085
      PrimOp.PextOp -> 0x00015086
      PrimOp.Clz8Op -> 0x00015087
      PrimOp.Clz16Op -> 0x00015088
      PrimOp.Clz32Op -> 0x00015089
      PrimOp.Clz64Op -> 0x0001508a
      PrimOp.ClzOp -> 0x0001508b
      PrimOp.Ctz8Op -> 0x0001508c
      PrimOp.Ctz16Op -> 0x0001508d
      PrimOp.Ctz32Op -> 0x0001508e
      PrimOp.Ctz64Op -> 0x0001508f
      PrimOp.CtzOp -> 0x00015090
      PrimOp.BSwap16Op -> 0x00015091
      PrimOp.BSwap32Op -> 0x00015092
      PrimOp.BSwap64Op -> 0x00015093
      PrimOp.BSwapOp -> 0x00015094
      PrimOp.Narrow8IntOp -> 0x00015095
      PrimOp.Narrow16IntOp -> 0x00015096
      PrimOp.Narrow32IntOp -> 0x00015097
      PrimOp.Narrow8WordOp -> 0x00015098
      PrimOp.Narrow16WordOp -> 0x00015099
      PrimOp.Narrow32WordOp -> 0x0001509a
      PrimOp.DoubleGtOp -> 0x0001509b
      PrimOp.DoubleGeOp -> 0x0001509c
      PrimOp.DoubleEqOp -> 0x0001509d
      PrimOp.DoubleNeOp -> 0x0001509e
      PrimOp.DoubleLtOp -> 0x0001509f
      PrimOp.DoubleLeOp -> 0x000150a0
      PrimOp.DoubleAddOp -> 0x000150a1
      PrimOp.DoubleSubOp -> 0x000150a2
      PrimOp.DoubleMulOp -> 0x000150a3
      PrimOp.DoubleDivOp -> 0x000150a4
      PrimOp.DoubleNegOp -> 0x000150a5
      PrimOp.DoubleFabsOp -> 0x000150a6
      PrimOp.Double2IntOp -> 0x000150a7
      PrimOp.Double2FloatOp -> 0x000150a8
      PrimOp.DoubleExpOp -> 0x000150a9
      PrimOp.DoubleLogOp -> 0x000150aa
      PrimOp.DoubleSqrtOp -> 0x000150ab
      PrimOp.DoubleSinOp -> 0x000150ac
      PrimOp.DoubleCosOp -> 0x000150ad
      PrimOp.DoubleTanOp -> 0x000150ae
      PrimOp.DoubleAsinOp -> 0x000150af
      PrimOp.DoubleAcosOp -> 0x000150b0
      PrimOp.DoubleAtanOp -> 0x000150b1
      PrimOp.DoubleSinhOp -> 0x000150b2
      PrimOp.DoubleCoshOp -> 0x000150b3
      PrimOp.DoubleTanhOp -> 0x000150b4
      PrimOp.DoubleAsinhOp -> 0x000150b5
      PrimOp.DoubleAcoshOp -> 0x000150b6
      PrimOp.DoubleAtanhOp -> 0x000150b7
      PrimOp.DoublePowerOp -> 0x000150b8
      PrimOp.DoubleDecode_2IntOp -> 0x000150b9
      PrimOp.DoubleDecode_Int64Op -> 0x000150ba
      PrimOp.FloatGtOp -> 0x000150bb
      PrimOp.FloatGeOp -> 0x000150bc
      PrimOp.FloatEqOp -> 0x000150bd
      PrimOp.FloatNeOp -> 0x000150be
      PrimOp.FloatLtOp -> 0x000150bf
      PrimOp.FloatLeOp -> 0x000150c0
      PrimOp.FloatAddOp -> 0x000150c1
      PrimOp.FloatSubOp -> 0x000150c2
      PrimOp.FloatMulOp -> 0x000150c3
      PrimOp.FloatDivOp -> 0x000150c4
      PrimOp.FloatNegOp -> 0x000150c5
      PrimOp.FloatFabsOp -> 0x000150c6
      PrimOp.Float2IntOp -> 0x000150c7
      PrimOp.FloatExpOp -> 0x000150c8
      PrimOp.FloatLogOp -> 0x000150c9
      PrimOp.FloatSqrtOp -> 0x000150ca
      PrimOp.FloatSinOp -> 0x000150cb
      PrimOp.FloatCosOp -> 0x000150cc
      PrimOp.FloatTanOp -> 0x000150cd
      PrimOp.FloatAsinOp -> 0x000150ce
      PrimOp.FloatAcosOp -> 0x000150cf
      PrimOp.FloatAtanOp -> 0x000150d0
      PrimOp.FloatSinhOp -> 0x000150d1
      PrimOp.FloatCoshOp -> 0x000150d2
      PrimOp.FloatTanhOp -> 0x000150d3
      PrimOp.FloatAsinhOp -> 0x000150d4
      PrimOp.FloatAcoshOp -> 0x000150d5
      PrimOp.FloatAtanhOp -> 0x000150d6
      PrimOp.FloatPowerOp -> 0x000150d7
      PrimOp.Float2DoubleOp -> 0x000150d8
      PrimOp.FloatDecode_IntOp -> 0x000150d9
      PrimOp.NewArrayOp -> 0x000150da
      PrimOp.SameMutableArrayOp -> 0x000150db
      PrimOp.ReadArrayOp -> 0x000150dc
      PrimOp.WriteArrayOp -> 0x000150dd
      PrimOp.SizeofArrayOp -> 0x000150de
      PrimOp.SizeofMutableArrayOp -> 0x000150df
      PrimOp.IndexArrayOp -> 0x000150e0
      PrimOp.UnsafeFreezeArrayOp -> 0x000150e1
      PrimOp.UnsafeThawArrayOp -> 0x000150e2
      PrimOp.CopyArrayOp -> 0x000150e3
      PrimOp.CopyMutableArrayOp -> 0x000150e4
      PrimOp.CloneArrayOp -> 0x000150e5
      PrimOp.CloneMutableArrayOp -> 0x000150e6
      PrimOp.FreezeArrayOp -> 0x000150e7
      PrimOp.ThawArrayOp -> 0x000150e8
      PrimOp.CasArrayOp -> 0x000150e9
      PrimOp.NewSmallArrayOp -> 0x000150ea
      PrimOp.SameSmallMutableArrayOp -> 0x000150eb
      PrimOp.ReadSmallArrayOp -> 0x000150ec
      PrimOp.WriteSmallArrayOp -> 0x000150ed
      PrimOp.SizeofSmallArrayOp -> 0x000150ee
      PrimOp.SizeofSmallMutableArrayOp -> 0x000150ef
      PrimOp.IndexSmallArrayOp -> 0x000150f0
      PrimOp.UnsafeFreezeSmallArrayOp -> 0x000150f1
      PrimOp.UnsafeThawSmallArrayOp -> 0x000150f2
      PrimOp.CopySmallArrayOp -> 0x000150f3
      PrimOp.CopySmallMutableArrayOp -> 0x000150f4
      PrimOp.CloneSmallArrayOp -> 0x000150f5
      PrimOp.CloneSmallMutableArrayOp -> 0x000150f6
      PrimOp.FreezeSmallArrayOp -> 0x000150f7
      PrimOp.ThawSmallArrayOp -> 0x000150f8
      PrimOp.CasSmallArrayOp -> 0x000150f9
      PrimOp.NewByteArrayOp_Char -> 0x000150fa
      PrimOp.NewPinnedByteArrayOp_Char -> 0x000150fb
      PrimOp.NewAlignedPinnedByteArrayOp_Char -> 0x000150fc
      PrimOp.MutableByteArrayIsPinnedOp -> 0x000150fd
      PrimOp.ByteArrayIsPinnedOp -> 0x000150fe
      PrimOp.ByteArrayContents_Char -> 0x000150ff
      PrimOp.SameMutableByteArrayOp -> 0x00015100
      PrimOp.ShrinkMutableByteArrayOp_Char -> 0x00015101
      PrimOp.ResizeMutableByteArrayOp_Char -> 0x00015102
      PrimOp.UnsafeFreezeByteArrayOp -> 0x00015103
      PrimOp.SizeofByteArrayOp -> 0x00015104
      PrimOp.SizeofMutableByteArrayOp -> 0x00015105
      PrimOp.GetSizeofMutableByteArrayOp -> 0x00015106
      PrimOp.IndexByteArrayOp_Char -> 0x00015107
      PrimOp.IndexByteArrayOp_WideChar -> 0x00015108
      PrimOp.IndexByteArrayOp_Int -> 0x00015109
      PrimOp.IndexByteArrayOp_Word -> 0x0001510a
      PrimOp.IndexByteArrayOp_Addr -> 0x0001510b
      PrimOp.IndexByteArrayOp_Float -> 0x0001510c
      PrimOp.IndexByteArrayOp_Double -> 0x0001510d
      PrimOp.IndexByteArrayOp_StablePtr -> 0x0001510e
      PrimOp.IndexByteArrayOp_Int8 -> 0x0001510f
      PrimOp.IndexByteArrayOp_Int16 -> 0x00015110
      PrimOp.IndexByteArrayOp_Int32 -> 0x00015111
      PrimOp.IndexByteArrayOp_Int64 -> 0x00015112
      PrimOp.IndexByteArrayOp_Word8 -> 0x00015113
      PrimOp.IndexByteArrayOp_Word16 -> 0x00015114
      PrimOp.IndexByteArrayOp_Word32 -> 0x00015115
      PrimOp.IndexByteArrayOp_Word64 -> 0x00015116
      PrimOp.IndexByteArrayOp_Word8AsChar -> 0x00015117
      PrimOp.IndexByteArrayOp_Word8AsWideChar -> 0x00015118
      PrimOp.IndexByteArrayOp_Word8AsAddr -> 0x00015119
      PrimOp.IndexByteArrayOp_Word8AsFloat -> 0x0001511a
      PrimOp.IndexByteArrayOp_Word8AsDouble -> 0x0001511b
      PrimOp.IndexByteArrayOp_Word8AsStablePtr -> 0x0001511c
      PrimOp.IndexByteArrayOp_Word8AsInt16 -> 0x0001511d
      PrimOp.IndexByteArrayOp_Word8AsInt32 -> 0x0001511e
      PrimOp.IndexByteArrayOp_Word8AsInt64 -> 0x0001511f
      PrimOp.IndexByteArrayOp_Word8AsInt -> 0x00015120
      PrimOp.IndexByteArrayOp_Word8AsWord16 -> 0x00015121
      PrimOp.IndexByteArrayOp_Word8AsWord32 -> 0x00015122
      PrimOp.IndexByteArrayOp_Word8AsWord64 -> 0x00015123
      PrimOp.IndexByteArrayOp_Word8AsWord -> 0x00015124
      PrimOp.ReadByteArrayOp_Char -> 0x00015125
      PrimOp.ReadByteArrayOp_WideChar -> 0x00015126
      PrimOp.ReadByteArrayOp_Int -> 0x00015127
      PrimOp.ReadByteArrayOp_Word -> 0x00015128
      PrimOp.ReadByteArrayOp_Addr -> 0x00015129
      PrimOp.ReadByteArrayOp_Float -> 0x0001512a
      PrimOp.ReadByteArrayOp_Double -> 0x0001512b
      PrimOp.ReadByteArrayOp_StablePtr -> 0x0001512c
      PrimOp.ReadByteArrayOp_Int8 -> 0x0001512d
      PrimOp.ReadByteArrayOp_Int16 -> 0x0001512e
      PrimOp.ReadByteArrayOp_Int32 -> 0x0001512f
      PrimOp.ReadByteArrayOp_Int64 -> 0x00015130
      PrimOp.ReadByteArrayOp_Word8 -> 0x00015131
      PrimOp.ReadByteArrayOp_Word16 -> 0x00015132
      PrimOp.ReadByteArrayOp_Word32 -> 0x00015133
      PrimOp.ReadByteArrayOp_Word64 -> 0x00015134
      PrimOp.ReadByteArrayOp_Word8AsChar -> 0x00015135
      PrimOp.ReadByteArrayOp_Word8AsWideChar -> 0x00015136
      PrimOp.ReadByteArrayOp_Word8AsAddr -> 0x00015137
      PrimOp.ReadByteArrayOp_Word8AsFloat -> 0x00015138
      PrimOp.ReadByteArrayOp_Word8AsDouble -> 0x00015139
      PrimOp.ReadByteArrayOp_Word8AsStablePtr -> 0x0001513a
      PrimOp.ReadByteArrayOp_Word8AsInt16 -> 0x0001513b
      PrimOp.ReadByteArrayOp_Word8AsInt32 -> 0x0001513c
      PrimOp.ReadByteArrayOp_Word8AsInt64 -> 0x0001513d
      PrimOp.ReadByteArrayOp_Word8AsInt -> 0x0001513e
      PrimOp.ReadByteArrayOp_Word8AsWord16 -> 0x0001513f
      PrimOp.ReadByteArrayOp_Word8AsWord32 -> 0x00015140
      PrimOp.ReadByteArrayOp_Word8AsWord64 -> 0x00015141
      PrimOp.ReadByteArrayOp_Word8AsWord -> 0x00015142
      PrimOp.WriteByteArrayOp_Char -> 0x00015143
      PrimOp.WriteByteArrayOp_WideChar -> 0x00015144
      PrimOp.WriteByteArrayOp_Int -> 0x00015145
      PrimOp.WriteByteArrayOp_Word -> 0x00015146
      PrimOp.WriteByteArrayOp_Addr -> 0x00015147
      PrimOp.WriteByteArrayOp_Float -> 0x00015148
      PrimOp.WriteByteArrayOp_Double -> 0x00015149
      PrimOp.WriteByteArrayOp_StablePtr -> 0x0001514a
      PrimOp.WriteByteArrayOp_Int8 -> 0x0001514b
      PrimOp.WriteByteArrayOp_Int16 -> 0x0001514c
      PrimOp.WriteByteArrayOp_Int32 -> 0x0001514d
      PrimOp.WriteByteArrayOp_Int64 -> 0x0001514e
      PrimOp.WriteByteArrayOp_Word8 -> 0x0001514f
      PrimOp.WriteByteArrayOp_Word16 -> 0x00015150
      PrimOp.WriteByteArrayOp_Word32 -> 0x00015151
      PrimOp.WriteByteArrayOp_Word64 -> 0x00015152
      PrimOp.WriteByteArrayOp_Word8AsChar -> 0x00015153
      PrimOp.WriteByteArrayOp_Word8AsWideChar -> 0x00015154
      PrimOp.WriteByteArrayOp_Word8AsAddr -> 0x00015155
      PrimOp.WriteByteArrayOp_Word8AsFloat -> 0x00015156
      PrimOp.WriteByteArrayOp_Word8AsDouble -> 0x00015157
      PrimOp.WriteByteArrayOp_Word8AsStablePtr -> 0x00015158
      PrimOp.WriteByteArrayOp_Word8AsInt16 -> 0x00015159
      PrimOp.WriteByteArrayOp_Word8AsInt32 -> 0x0001515a
      PrimOp.WriteByteArrayOp_Word8AsInt64 -> 0x0001515b
      PrimOp.WriteByteArrayOp_Word8AsInt -> 0x0001515c
      PrimOp.WriteByteArrayOp_Word8AsWord16 -> 0x0001515d
      PrimOp.WriteByteArrayOp_Word8AsWord32 -> 0x0001515e
      PrimOp.WriteByteArrayOp_Word8AsWord64 -> 0x0001515f
      PrimOp.WriteByteArrayOp_Word8AsWord -> 0x00015160
      PrimOp.CompareByteArraysOp -> 0x00015161
      PrimOp.CopyByteArrayOp -> 0x00015162
      PrimOp.CopyMutableByteArrayOp -> 0x00015163
      PrimOp.CopyByteArrayToAddrOp -> 0x00015164
      PrimOp.CopyMutableByteArrayToAddrOp -> 0x00015165
      PrimOp.CopyAddrToByteArrayOp -> 0x00015166
      PrimOp.SetByteArrayOp -> 0x00015167
      PrimOp.AtomicReadByteArrayOp_Int -> 0x00015168
      PrimOp.AtomicWriteByteArrayOp_Int -> 0x00015169
      PrimOp.CasByteArrayOp_Int -> 0x0001516a
      PrimOp.FetchAddByteArrayOp_Int -> 0x0001516b
      PrimOp.FetchSubByteArrayOp_Int -> 0x0001516c
      PrimOp.FetchAndByteArrayOp_Int -> 0x0001516d
      PrimOp.FetchNandByteArrayOp_Int -> 0x0001516e
      PrimOp.FetchOrByteArrayOp_Int -> 0x0001516f
      PrimOp.FetchXorByteArrayOp_Int -> 0x00015170
      PrimOp.NewArrayArrayOp -> 0x00015171
      PrimOp.SameMutableArrayArrayOp -> 0x00015172
      PrimOp.UnsafeFreezeArrayArrayOp -> 0x00015173
      PrimOp.SizeofArrayArrayOp -> 0x00015174
      PrimOp.SizeofMutableArrayArrayOp -> 0x00015175
      PrimOp.IndexArrayArrayOp_ByteArray -> 0x00015176
      PrimOp.IndexArrayArrayOp_ArrayArray -> 0x00015177
      PrimOp.ReadArrayArrayOp_ByteArray -> 0x00015178
      PrimOp.ReadArrayArrayOp_MutableByteArray -> 0x00015179
      PrimOp.ReadArrayArrayOp_ArrayArray -> 0x0001517a
      PrimOp.ReadArrayArrayOp_MutableArrayArray -> 0x0001517b
      PrimOp.WriteArrayArrayOp_ByteArray -> 0x0001517c
      PrimOp.WriteArrayArrayOp_MutableByteArray -> 0x0001517d
      PrimOp.WriteArrayArrayOp_ArrayArray -> 0x0001517e
      PrimOp.WriteArrayArrayOp_MutableArrayArray -> 0x0001517f
      PrimOp.CopyArrayArrayOp -> 0x00015180
      PrimOp.CopyMutableArrayArrayOp -> 0x00015181
      PrimOp.AddrAddOp -> 0x00015182
      PrimOp.AddrSubOp -> 0x00015183
      PrimOp.AddrRemOp -> 0x00015184
      PrimOp.Addr2IntOp -> 0x00015185
      PrimOp.Int2AddrOp -> 0x00015186
      PrimOp.AddrGtOp -> 0x00015187
      PrimOp.AddrGeOp -> 0x00015188
      PrimOp.AddrEqOp -> 0x00015189
      PrimOp.AddrNeOp -> 0x0001518a
      PrimOp.AddrLtOp -> 0x0001518b
      PrimOp.AddrLeOp -> 0x0001518c
      PrimOp.IndexOffAddrOp_Char -> 0x0001518d
      PrimOp.IndexOffAddrOp_WideChar -> 0x0001518e
      PrimOp.IndexOffAddrOp_Int -> 0x0001518f
      PrimOp.IndexOffAddrOp_Word -> 0x00015190
      PrimOp.IndexOffAddrOp_Addr -> 0x00015191
      PrimOp.IndexOffAddrOp_Float -> 0x00015192
      PrimOp.IndexOffAddrOp_Double -> 0x00015193
      PrimOp.IndexOffAddrOp_StablePtr -> 0x00015194
      PrimOp.IndexOffAddrOp_Int8 -> 0x00015195
      PrimOp.IndexOffAddrOp_Int16 -> 0x00015196
      PrimOp.IndexOffAddrOp_Int32 -> 0x00015197
      PrimOp.IndexOffAddrOp_Int64 -> 0x00015198
      PrimOp.IndexOffAddrOp_Word8 -> 0x00015199
      PrimOp.IndexOffAddrOp_Word16 -> 0x0001519a
      PrimOp.IndexOffAddrOp_Word32 -> 0x0001519b
      PrimOp.IndexOffAddrOp_Word64 -> 0x0001519c
      PrimOp.ReadOffAddrOp_Char -> 0x0001519d
      PrimOp.ReadOffAddrOp_WideChar -> 0x0001519e
      PrimOp.ReadOffAddrOp_Int -> 0x0001519f
      PrimOp.ReadOffAddrOp_Word -> 0x000151a0
      PrimOp.ReadOffAddrOp_Addr -> 0x000151a1
      PrimOp.ReadOffAddrOp_Float -> 0x000151a2
      PrimOp.ReadOffAddrOp_Double -> 0x000151a3
      PrimOp.ReadOffAddrOp_StablePtr -> 0x000151a4
      PrimOp.ReadOffAddrOp_Int8 -> 0x000151a5
      PrimOp.ReadOffAddrOp_Int16 -> 0x000151a6
      PrimOp.ReadOffAddrOp_Int32 -> 0x000151a7
      PrimOp.ReadOffAddrOp_Int64 -> 0x000151a8
      PrimOp.ReadOffAddrOp_Word8 -> 0x000151a9
      PrimOp.ReadOffAddrOp_Word16 -> 0x000151aa
      PrimOp.ReadOffAddrOp_Word32 -> 0x000151ab
      PrimOp.ReadOffAddrOp_Word64 -> 0x000151ac
      PrimOp.WriteOffAddrOp_Char -> 0x000151ad
      PrimOp.WriteOffAddrOp_WideChar -> 0x000151ae
      PrimOp.WriteOffAddrOp_Int -> 0x000151af
      PrimOp.WriteOffAddrOp_Word -> 0x000151b0
      PrimOp.WriteOffAddrOp_Addr -> 0x000151b1
      PrimOp.WriteOffAddrOp_Float -> 0x000151b2
      PrimOp.WriteOffAddrOp_Double -> 0x000151b3
      PrimOp.WriteOffAddrOp_StablePtr -> 0x000151b4
      PrimOp.WriteOffAddrOp_Int8 -> 0x000151b5
      PrimOp.WriteOffAddrOp_Int16 -> 0x000151b6
      PrimOp.WriteOffAddrOp_Int32 -> 0x000151b7
      PrimOp.WriteOffAddrOp_Int64 -> 0x000151b8
      PrimOp.WriteOffAddrOp_Word8 -> 0x000151b9
      PrimOp.WriteOffAddrOp_Word16 -> 0x000151ba
      PrimOp.WriteOffAddrOp_Word32 -> 0x000151bb
      PrimOp.WriteOffAddrOp_Word64 -> 0x000151bc
      PrimOp.NewMutVarOp -> 0x000151bd
      PrimOp.ReadMutVarOp -> 0x000151be
      PrimOp.WriteMutVarOp -> 0x000151bf
      PrimOp.SameMutVarOp -> 0x000151c0
      PrimOp.AtomicModifyMutVar2Op -> 0x000151c1
      PrimOp.AtomicModifyMutVar_Op -> 0x000151c2
      PrimOp.CasMutVarOp -> 0x000151c3
      PrimOp.CatchOp -> 0x000151c4
      PrimOp.RaiseOp -> 0x000151c5
      PrimOp.RaiseIOOp -> 0x000151c6
      PrimOp.MaskAsyncExceptionsOp -> 0x000151c7
      PrimOp.MaskUninterruptibleOp -> 0x000151c8
      PrimOp.UnmaskAsyncExceptionsOp -> 0x000151c9
      PrimOp.MaskStatus -> 0x000151ca
      PrimOp.AtomicallyOp -> 0x000151cb
      PrimOp.RetryOp -> 0x000151cc
      PrimOp.CatchRetryOp -> 0x000151cd
      PrimOp.CatchSTMOp -> 0x000151ce
      PrimOp.NewTVarOp -> 0x000151cf
      PrimOp.ReadTVarOp -> 0x000151d0
      PrimOp.ReadTVarIOOp -> 0x000151d1
      PrimOp.WriteTVarOp -> 0x000151d2
      PrimOp.SameTVarOp -> 0x000151d3
      PrimOp.NewMVarOp -> 0x000151d4
      PrimOp.TakeMVarOp -> 0x000151d5
      PrimOp.TryTakeMVarOp -> 0x000151d6
      PrimOp.PutMVarOp -> 0x000151d7
      PrimOp.TryPutMVarOp -> 0x000151d8
      PrimOp.ReadMVarOp -> 0x000151d9
      PrimOp.TryReadMVarOp -> 0x000151da
      PrimOp.SameMVarOp -> 0x000151db
      PrimOp.IsEmptyMVarOp -> 0x000151dc
      PrimOp.DelayOp -> 0x000151dd
      PrimOp.WaitReadOp -> 0x000151de
      PrimOp.WaitWriteOp -> 0x000151df
      PrimOp.ForkOp -> 0x000151e0
      PrimOp.ForkOnOp -> 0x000151e1
      PrimOp.KillThreadOp -> 0x000151e2
      PrimOp.YieldOp -> 0x000151e3
      PrimOp.MyThreadIdOp -> 0x000151e4
      PrimOp.LabelThreadOp -> 0x000151e5
      PrimOp.IsCurrentThreadBoundOp -> 0x000151e6
      PrimOp.NoDuplicateOp -> 0x000151e7
      PrimOp.ThreadStatusOp -> 0x000151e8
      PrimOp.MkWeakOp -> 0x000151e9
      PrimOp.MkWeakNoFinalizerOp -> 0x000151ea
      PrimOp.AddCFinalizerToWeakOp -> 0x000151eb
      PrimOp.DeRefWeakOp -> 0x000151ec
      PrimOp.FinalizeWeakOp -> 0x000151ed
      PrimOp.TouchOp -> 0x000151ee
      PrimOp.MakeStablePtrOp -> 0x000151ef
      PrimOp.DeRefStablePtrOp -> 0x000151f0
      PrimOp.EqStablePtrOp -> 0x000151f1
      PrimOp.MakeStableNameOp -> 0x000151f2
      PrimOp.EqStableNameOp -> 0x000151f3
      PrimOp.StableNameToIntOp -> 0x000151f4
      PrimOp.CompactNewOp -> 0x000151f5
      PrimOp.CompactResizeOp -> 0x000151f6
      PrimOp.CompactContainsOp -> 0x000151f7
      PrimOp.CompactContainsAnyOp -> 0x000151f8
      PrimOp.CompactGetFirstBlockOp -> 0x000151f9
      PrimOp.CompactGetNextBlockOp -> 0x000151fa
      PrimOp.CompactAllocateBlockOp -> 0x000151fb
      PrimOp.CompactFixupPointersOp -> 0x000151fc
      PrimOp.CompactAdd -> 0x000151fd
      PrimOp.CompactAddWithSharing -> 0x000151fe
      PrimOp.CompactSize -> 0x000151ff
      PrimOp.ReallyUnsafePtrEqualityOp -> 0x00015200
      PrimOp.ParOp -> 0x00015201
      PrimOp.SparkOp -> 0x00015202
      PrimOp.SeqOp -> 0x00015203
      PrimOp.GetSparkOp -> 0x00015204
      PrimOp.NumSparks -> 0x00015205
      PrimOp.DataToTagOp -> 0x00015206
      PrimOp.TagToEnumOp -> 0x00015207
      PrimOp.AddrToAnyOp -> 0x00015208
      PrimOp.AnyToAddrOp -> 0x00015209
      PrimOp.MkApUpd0_Op -> 0x0001520a
      PrimOp.NewBCOOp -> 0x0001520b
      PrimOp.UnpackClosureOp -> 0x0001520c
      PrimOp.GetApStackValOp -> 0x0001520d
      PrimOp.GetCCSOfOp -> 0x0001520e
      PrimOp.GetCurrentCCSOp -> 0x0001520f
      PrimOp.ClearCCSOp -> 0x00015210
      PrimOp.TraceEventOp -> 0x00015211
      PrimOp.TraceEventBinaryOp -> 0x00015212
      PrimOp.TraceMarkerOp -> 0x00015213
      -- PrimOp.GetThreadAllocationCounter -> 0x00015214
      PrimOp.SetThreadAllocationCounter -> 0x00015215
      PrimOp.VecBroadcastOp _ _ _ -> 0x00015216
      PrimOp.VecPackOp _ _ _ -> 0x00015217
      PrimOp.VecUnpackOp _ _ _ -> 0x00015218
      PrimOp.VecInsertOp _ _ _ -> 0x00015219
      PrimOp.VecAddOp _ _ _ -> 0x0001521a
      PrimOp.VecSubOp _ _ _ -> 0x0001521b
      PrimOp.VecMulOp _ _ _ -> 0x0001521c
      PrimOp.VecDivOp _ _ _ -> 0x0001521d
      PrimOp.VecQuotOp _ _ _ -> 0x0001521e
      PrimOp.VecRemOp _ _ _ -> 0x0001521f
      PrimOp.VecNegOp _ _ _ -> 0x00015220
      PrimOp.VecIndexByteArrayOp _ _ _ -> 0x00015221
      PrimOp.VecReadByteArrayOp _ _ _ -> 0x00015222
      PrimOp.VecWriteByteArrayOp _ _ _ -> 0x00015223
      PrimOp.VecIndexOffAddrOp _ _ _ -> 0x00015224
      PrimOp.VecReadOffAddrOp _ _ _ -> 0x00015225
      PrimOp.VecWriteOffAddrOp _ _ _ -> 0x00015226
      PrimOp.VecIndexScalarByteArrayOp _ _ _ -> 0x00015227
      PrimOp.VecReadScalarByteArrayOp _ _ _ -> 0x00015228
      PrimOp.VecWriteScalarByteArrayOp _ _ _ -> 0x00015229
      PrimOp.VecIndexScalarOffAddrOp _ _ _ -> 0x0001522a
      PrimOp.VecReadScalarOffAddrOp _ _ _ -> 0x0001522b
      PrimOp.VecWriteScalarOffAddrOp _ _ _ -> 0x0001522c
      PrimOp.PrefetchByteArrayOp3 -> 0x0001522d
      PrimOp.PrefetchMutableByteArrayOp3 -> 0x0001522e
      PrimOp.PrefetchAddrOp3 -> 0x0001522f
      PrimOp.PrefetchValueOp3 -> 0x00015230
      PrimOp.PrefetchByteArrayOp2 -> 0x00015231
      PrimOp.PrefetchMutableByteArrayOp2 -> 0x00015232
      PrimOp.PrefetchAddrOp2 -> 0x00015233
      PrimOp.PrefetchValueOp2 -> 0x00015234
      PrimOp.PrefetchByteArrayOp1 -> 0x00015235
      PrimOp.PrefetchMutableByteArrayOp1 -> 0x00015236
      PrimOp.PrefetchAddrOp1 -> 0x00015237
      PrimOp.PrefetchValueOp1 -> 0x00015238
      PrimOp.PrefetchByteArrayOp0 -> 0x00015239
      PrimOp.PrefetchMutableByteArrayOp0 -> 0x0001523a
      PrimOp.PrefetchAddrOp0 -> 0x0001523b
      PrimOp.PrefetchValueOp0 -> 0x0001523c

    uniqueBytes x = bytes where
        tb = typeID' x
        bts = case x of
          PrimOp.VecBroadcastOp cat l w -> toBytes cat ++ toBytes l ++ toBytes w
          PrimOp.VecPackOp cat l w -> toBytes cat ++ toBytes l ++ toBytes w
          PrimOp.VecUnpackOp cat l w -> toBytes cat ++ toBytes l ++ toBytes w
          PrimOp.VecInsertOp cat l w -> toBytes cat ++ toBytes l ++ toBytes w
          PrimOp.VecAddOp cat l w -> toBytes cat ++ toBytes l ++ toBytes w
          PrimOp.VecSubOp cat l w -> toBytes cat ++ toBytes l ++ toBytes w
          PrimOp.VecMulOp cat l w -> toBytes cat ++ toBytes l ++ toBytes w
          PrimOp.VecDivOp cat l w -> toBytes cat ++ toBytes l ++ toBytes w
          PrimOp.VecQuotOp cat l w -> toBytes cat ++ toBytes l ++ toBytes w
          PrimOp.VecRemOp cat l w -> toBytes cat ++ toBytes l ++ toBytes w
          PrimOp.VecNegOp cat l w -> toBytes cat ++ toBytes l ++ toBytes w
          PrimOp.VecIndexByteArrayOp cat l w -> toBytes cat ++ toBytes l ++ toBytes w
          PrimOp.VecReadByteArrayOp cat l w -> toBytes cat ++ toBytes l ++ toBytes w
          PrimOp.VecWriteByteArrayOp cat l w -> toBytes cat ++ toBytes l ++ toBytes w
          PrimOp.VecIndexOffAddrOp cat l w -> toBytes cat ++ toBytes l ++ toBytes w
          PrimOp.VecReadOffAddrOp cat l w -> toBytes cat ++ toBytes l ++ toBytes w
          PrimOp.VecWriteOffAddrOp cat l w -> toBytes cat ++ toBytes l ++ toBytes w
          PrimOp.VecIndexScalarByteArrayOp cat l w -> toBytes cat ++ toBytes l ++ toBytes w
          PrimOp.VecReadScalarByteArrayOp cat l w -> toBytes cat ++ toBytes l ++ toBytes w
          PrimOp.VecWriteScalarByteArrayOp cat l w -> toBytes cat ++ toBytes l ++ toBytes w
          PrimOp.VecIndexScalarOffAddrOp cat l w -> toBytes cat ++ toBytes l ++ toBytes w
          PrimOp.VecReadScalarOffAddrOp cat l w -> toBytes cat ++ toBytes l ++ toBytes w
          PrimOp.VecWriteScalarOffAddrOp cat l w -> toBytes cat ++ toBytes l ++ toBytes w
          _ -> []
        lb = toBytes (sum $ map (BS.length) bts)
        bytes = tb ++ lb ++ bts

instance BinarySerializable PrimOp.PrimOpVecCat where
    toBytes x = toBytes b where
        b = case x of
          PrimOp.IntVec   -> 0x00 :: Word8
          PrimOp.WordVec  -> 0x01
          PrimOp.FloatVec -> 0x02

instance BinarySerializable CmmType.Width where
    toBytes x = toBytes b where
        b = case x of
          CmmType.W8 -> 0x08 :: Word16
          CmmType.W16 -> 0x10
          CmmType.W32 -> 0x20
          CmmType.W64 -> 0x40
          -- CmmType.W80 -> 0x50
          CmmType.W128 -> 0x80
          CmmType.W256 -> 0x100
          CmmType.W512 -> 0x200
