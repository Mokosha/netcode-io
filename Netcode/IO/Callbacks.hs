module Netcode.IO.Callbacks where

--------------------------------------------------------------------------------
import Data.Word             (Word8)
import Foreign.C.Types       (CInt)
import Foreign.ForeignPtr    (newForeignPtr_)
import Foreign.Ptr           (Ptr)

import Bindings.Netcode.IO
import Netcode.IO.Address

--------------------------------------------------------------------------------

type SendPacketOverride = Address -> Ptr Word8 -> CInt -> IO ()

mkSendPacketOverride :: SendPacketOverride -> IO C'send_packet_override_t
mkSendPacketOverride sendFn =
    mk'send_packet_override_t $ \_ aptr pkt pktSize -> do
        addr <- Address <$> newForeignPtr_ aptr
        sendFn addr pkt pktSize

type ReceivePacketOverride = Address -> Ptr Word8 -> CInt -> IO CInt

mkReceivePacketOverride :: ReceivePacketOverride
                        -> IO C'receive_packet_override_t
mkReceivePacketOverride recvFn =
    mk'receive_packet_override_t $ \_ aptr pkt pktSize -> do
        addr <- Address <$> newForeignPtr_ aptr
        recvFn addr pkt pktSize
    