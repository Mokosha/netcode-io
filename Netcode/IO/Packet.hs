module Netcode.IO.Packet where

--------------------------------------------------------------------------------

import Data.Word (Word8, Word64)
import Foreign.ForeignPtr (ForeignPtr)

import Bindings.Netcode.IO

--------------------------------------------------------------------------------

data Packet = Packet {
    packetSequenceNumber :: Word64,
    packetSize :: Int,
    packetDataPtr :: ForeignPtr Word8
}

maximumPacketSize :: Num a => a
maximumPacketSize = c'NETCODE_MAX_PACKET_SIZE