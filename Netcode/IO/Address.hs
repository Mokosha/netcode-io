module Netcode.IO.Address where

--------------------------------------------------------------------------------

import Control.Monad         (unless)
import Foreign.C.String      (withCString, peekCString)
import Foreign.ForeignPtr    (ForeignPtr, withForeignPtr, mallocForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes)

import Bindings.Netcode.IO

--------------------------------------------------------------------------------

newtype Address = Address (ForeignPtr C'netcode_address_t)
    deriving (Show)

parseAddress :: String -> IO Address
parseAddress addrStr = do
    address <- mallocForeignPtr
    retVal <- withForeignPtr address $ \addressPtr ->
                withCString addrStr (`c'netcode_parse_address` addressPtr)
    unless (retVal == c'NETCODE_OK) $ fail "Unable to parse address"
    return $ Address address

addressToString :: Address -> IO String
addressToString (Address addrPtr) =
    let maxAddrStringLen = 256
     in allocaBytes maxAddrStringLen $ \addrStr -> do
         _ <- withForeignPtr addrPtr (`c'netcode_address_to_string` addrStr)
         peekCString addrStr

addressEqual :: Address -> Address -> IO Bool
addressEqual (Address addrPtrA) (Address addrPtrB) =
    withForeignPtr addrPtrA $ \ptrA ->
        withForeignPtr addrPtrB $ \ptrB ->
            (/= 0) <$> c'netcode_address_equal ptrA ptrB

