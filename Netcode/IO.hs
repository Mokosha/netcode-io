{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Netcode.IO (
      initialize
    , terminate

    , Address
    , parseAddress
    , addressToString
    , addressEqual

    , SendPacketOverride
    , ReceivePacketOverride

    , module Netcode.IO.Packet
    , module Netcode.IO.Client
    , module Netcode.IO.Server

    , sleep

    , LogLevel(..), logLevel
) where
--------------------------------------------------------------------------------

import Control.Monad         (when)
import Data.Data             (Data)
import Data.Typeable         (Typeable)
import Foreign.C.Types       (CDouble(..))
import GHC.Generics          (Generic)

import Bindings.Netcode.IO
import Netcode.IO.Address
import Netcode.IO.Callbacks
import Netcode.IO.Client
import Netcode.IO.Packet
import Netcode.IO.Server

--------------------------------------------------------------------------------

-- | Initializes the netcode.io library. Throws an IOException on failure.
initialize :: IO ()
initialize = do
    result <- c'netcode_init
    when (result == c'NETCODE_OK) $ fail "Failed to initialize netcode.io"
    return ()

terminate :: IO ()
terminate = c'netcode_term

data LogLevel = LogLevel'None
              | LogLevel'Info
              | LogLevel'Error
              | LogLevel'Debug
    deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, Data, Typeable)

logLevel :: LogLevel -> IO ()
logLevel LogLevel'None  = c'netcode_log_level c'NETCODE_LOG_LEVEL_NONE
logLevel LogLevel'Info  = c'netcode_log_level c'NETCODE_LOG_LEVEL_INFO
logLevel LogLevel'Error = c'netcode_log_level c'NETCODE_LOG_LEVEL_ERROR
logLevel LogLevel'Debug = c'netcode_log_level c'NETCODE_LOG_LEVEL_DEBUG

sleep :: Double -> IO ()
sleep = c'netcode_sleep . CDouble
