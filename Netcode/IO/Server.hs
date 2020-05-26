module Netcode.IO.Server (
      ServerConfig
    , defaultServerConfig
    , setProtocolID
    , setPrivateKey
    , ServerConnectDisconnectCallback
    , setServerConnectDisconnectCallback, clearServerConnectDisconnectCallback
    , setServerSendReceiveOverrides, clearServerSendReceiveOverrides

    , Server
    , createServer
    , destroyServer
    , startServer
    , maxNumClients
    , stopServer
    , updateServer
    , clientConnectedAtIndex
    , clientIdAtIndex
    , withClientAddressAtIndex
    , withClientUserDataAtIndex
    , clientUserDataAtIndex
    , maxClientsForServer
    , numConnectedClients
    , isServerRunning
    , isServerFull
    , getServerPort
    , sendPacketFromServer
    , disconnectClientFromServer
    , disconnectAllClientsFromServer
    , receivePacketFromClient
    , nextServerPacketSequence
) where

--------------------------------------------------------------------------------

import Control.Applicative   (liftA2)
import Control.Monad         (when)
import Data.Word             (Word8, Word16, Word64)
import Foreign.C.String      (withCString)
import Foreign.C.Types       (CDouble(..))
import Foreign.Concurrent    (newForeignPtr)
import Foreign.ForeignPtr    (newForeignPtr_)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr           ( Ptr, nullPtr, castPtr
                             , FunPtr, nullFunPtr, freeHaskellFunPtr
                             )
import Foreign.Storable      (peek, poke)

import Bindings.Netcode.IO
import Netcode.IO.Address
import Netcode.IO.Callbacks
import Netcode.IO.Packet

--------------------------------------------------------------------------------

freeNullFunPtr :: FunPtr a -> IO ()
freeNullFunPtr x
    | x == nullFunPtr = return ()
    | otherwise       = freeHaskellFunPtr x

--------------------------------------------------------------------------------

data Server = Server 
    { serverPtr :: Ptr C'netcode_server_t
    , serverCallbacks :: ServerCallbacks
    } deriving (Show)

data ServerCallbacks = ServerCallbacks
    { serverConnectDisconnect     :: C'connect_disconnect_callback_t
    , serverSendPacketOverride    :: C'send_packet_override_t
    , serverReceivePacketOverride :: C'receive_packet_override_t
    } deriving (Show)

defaultServerCallbacks :: ServerCallbacks
defaultServerCallbacks = ServerCallbacks nullFunPtr nullFunPtr nullFunPtr

newtype ServerConfig = ServerConfig
    (   Ptr C'netcode_server_config_t
        -> ServerCallbacks
        -> IO (Ptr C'netcode_server_config_t, ServerCallbacks)
    )

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig $ \serverConfig cbs -> do
    c'netcode_default_server_config serverConfig
    return (serverConfig, cbs)

setProtocolID :: Word64 -> ServerConfig -> ServerConfig
setProtocolID protocolID (ServerConfig mkServerPtr) =
    ServerConfig $ \serverConfig cbs' -> do
        (configPtr, cbs) <- mkServerPtr serverConfig cbs'
        config <- peek configPtr
        poke configPtr $
            config { c'netcode_server_config_t'protocol_id = protocolID }
        return (configPtr, cbs)

setPrivateKey :: [Word8] -> ServerConfig -> ServerConfig
setPrivateKey key (ServerConfig mkServerPtr) =
    ServerConfig $ \serverConfig cbs' -> do
        (configPtr, cbs) <- mkServerPtr serverConfig cbs'
        config <- peek configPtr
        poke configPtr $    
            config {
                c'netcode_server_config_t'private_key = 
                    take c'NETCODE_KEY_BYTES (key <> repeat 0)
            }
        return (configPtr, cbs)

type ServerConnectDisconnectCallback = Int -> Bool -> IO ()

mkServerConnectDisconnectCallback :: ServerConnectDisconnectCallback
                                  -> IO C'connect_disconnect_callback_t
mkServerConnectDisconnectCallback cb = mk'connect_disconnect_callback_t $
    \_ clientIdx connected -> cb (fromIntegral clientIdx) (connected /= 0)

setServerConnectDisconnectCallback :: ServerConnectDisconnectCallback
                                    -> ServerConfig -> ServerConfig
setServerConnectDisconnectCallback cb (ServerConfig mkConfig) =
    ServerConfig $ \configPtr' callbacks' -> do
        (configPtr, callbacks) <- mkConfig configPtr' callbacks'
        freeNullFunPtr $ serverConnectDisconnect callbacks
        fPtr <- mkServerConnectDisconnectCallback cb
        config <- peek configPtr
        poke configPtr $ config
            { c'netcode_server_config_t'connect_disconnect_callback = fPtr
            }
        return (configPtr, callbacks { serverConnectDisconnect = fPtr })

clearServerConnectDisconnectCallback :: ServerConfig -> ServerConfig
clearServerConnectDisconnectCallback (ServerConfig mkConfig) =
    ServerConfig $ \configPtr' callbacks' -> do
        (configPtr, callbacks) <- mkConfig configPtr' callbacks'
        freeNullFunPtr $ serverConnectDisconnect callbacks
        config <- peek configPtr
        poke configPtr $ config
            { c'netcode_server_config_t'connect_disconnect_callback = nullFunPtr
            }
        return (configPtr, callbacks { serverConnectDisconnect = nullFunPtr })

setServerSendReceiveOverrides :: SendPacketOverride
                            -> ReceivePacketOverride
                            -> ServerConfig -> ServerConfig
setServerSendReceiveOverrides sendFn recvFn (ServerConfig mkConfig) =
    ServerConfig $ \configPtr' callbacks' -> do
        (configPtr, callbacks) <- mkConfig configPtr' callbacks'
        freeNullFunPtr $ serverSendPacketOverride callbacks
        freeNullFunPtr $ serverReceivePacketOverride callbacks
        config <- peek configPtr
        sendOverride <- mkSendPacketOverride sendFn
        recvOverride <- mkReceivePacketOverride recvFn
        poke configPtr $ config
            { c'netcode_server_config_t'send_packet_override = sendOverride
            , c'netcode_server_config_t'receive_packet_override = recvOverride
            , c'netcode_server_config_t'override_send_and_receive = 1
            }
        let newcbs = callbacks
                { serverSendPacketOverride = sendOverride
                , serverReceivePacketOverride = recvOverride
                }
        return (configPtr, newcbs)

clearServerSendReceiveOverrides :: ServerConfig -> ServerConfig
clearServerSendReceiveOverrides (ServerConfig mkConfig) =
    ServerConfig $ \configPtr' callbacks' -> do
        (configPtr, callbacks) <- mkConfig configPtr' callbacks'
        freeNullFunPtr $ serverSendPacketOverride callbacks
        freeNullFunPtr $ serverReceivePacketOverride callbacks
        config <- peek configPtr
        poke configPtr $ config
            { c'netcode_server_config_t'send_packet_override = nullFunPtr
            , c'netcode_server_config_t'receive_packet_override = nullFunPtr
            , c'netcode_server_config_t'override_send_and_receive = 0
            }
        let newcbs = callbacks
                { serverSendPacketOverride = nullFunPtr
                , serverReceivePacketOverride = nullFunPtr
                }
        return (configPtr, newcbs)

-- | Creates a server at the given address using the provided config. Throws an
-- IOException on failure.
createServer :: String -> ServerConfig -> Double -> IO Server
createServer s (ServerConfig mkConfig) time = alloca $ \serverConfig -> do
    (config, cbs) <- mkConfig serverConfig defaultServerCallbacks
    ptr <- withCString s (\cs -> c'netcode_server_create cs config (CDouble time))
    when (ptr == nullPtr) $ fail "Failed to create server!"
    return (Server ptr cbs)

-- | Starts the server and specifies the maximum number of clients that can
-- connect.
startServer :: Server -> Int -> IO ()
startServer (Server s _) = c'netcode_server_start s . fromIntegral

maxNumClients :: Num a => a
maxNumClients = c'NETCODE_MAX_CLIENTS

-- | Stops the server.
stopServer :: Server -> IO ()
stopServer (Server s _) = c'netcode_server_stop s

destroyServer :: Server -> IO ()
destroyServer (Server s cbs) = do
    c'netcode_server_destroy s
    freeNullFunPtr $ serverConnectDisconnect cbs
    freeNullFunPtr $ serverSendPacketOverride cbs
    freeNullFunPtr $ serverReceivePacketOverride cbs

updateServer :: Server -> Double -> IO ()
updateServer (Server s _) = c'netcode_server_update s . CDouble

clientConnectedAtIndex :: Server -> Int -> IO Bool
clientConnectedAtIndex (Server s _) =
    fmap (/= 0) . c'netcode_server_client_connected s . fromIntegral

clientIdAtIndex :: Server -> Int -> IO Word64
clientIdAtIndex (Server s _) = c'netcode_server_client_id s . fromIntegral

-- Note, the address here shouldn't outlive the connected client.
withClientAddressAtIndex :: Server -> Int -> (Address -> IO a) -> IO a
withClientAddressAtIndex (Server s _) cidx fn = do
    aptr <- c'netcode_server_client_address s (fromIntegral cidx)
    Address <$> newForeignPtr_ aptr >>= fn

withClientUserDataAtIndex :: Server -> Int -> (Ptr () -> IO a) -> IO a
withClientUserDataAtIndex (Server s _) cidx fn =
    c'netcode_server_client_user_data s (fromIntegral cidx) >>= fn

clientUserDataAtIndex :: Server -> Int -> IO [Word8]
clientUserDataAtIndex s i = withClientUserDataAtIndex s i $
    peekArray c'NETCODE_USER_DATA_BYTES . castPtr

maxClientsForServer :: Server -> IO Int
maxClientsForServer (Server s _) =
    fromIntegral <$> c'netcode_server_max_clients s

numConnectedClients :: Server -> IO Int
numConnectedClients (Server s _) =
    fromIntegral <$> c'netcode_server_num_connected_clients s

isServerRunning :: Server -> IO Bool
isServerRunning (Server s _) = (/= 0) <$> c'netcode_server_running s

isServerFull :: Server -> IO Bool
isServerFull (Server s _) =
    liftA2 (==) (c'netcode_server_num_connected_clients s)
                (c'netcode_server_max_clients s)

getServerPort :: Server -> IO Word16
getServerPort (Server s _) = c'netcode_server_get_port s

disconnectClientFromServer :: Server -> Int -> IO ()
disconnectClientFromServer (Server s _) =
    c'netcode_server_disconnect_client s . fromIntegral

disconnectAllClientsFromServer :: Server -> IO ()
disconnectAllClientsFromServer (Server s _) =
    c'netcode_server_disconnect_all_clients s

nextServerPacketSequence :: Server -> Int -> IO Word64
nextServerPacketSequence (Server s _) =
    c'netcode_server_next_packet_sequence s . fromIntegral

sendPacketFromServer :: Server -> Int -> Int -> Ptr Word8 -> IO ()
sendPacketFromServer (Server s _) clientIdx pktSz pktMem = do
    let pktSize = min c'NETCODE_MAX_PACKET_SIZE (fromIntegral pktSz)
    when (pktSz > c'NETCODE_MAX_PACKET_SIZE) $ putStrLn $
            "WARNING: Sending packet that's too large: " <> show pktSz
    c'netcode_server_send_packet s (fromIntegral clientIdx) pktMem pktSize

receivePacketFromClient :: Server -> Int -> IO (Maybe Packet)
receivePacketFromClient (Server s _) clientIdx =
    alloca $ \sequenceNumPtr ->
    alloca $ \pktSzPtr -> do
        packetMem <- c'netcode_server_receive_packet s (fromIntegral clientIdx) pktSzPtr sequenceNumPtr
        if packetMem == nullPtr
            then return Nothing
            else fmap Just $
                Packet <$> peek sequenceNumPtr
                        <*> (fromIntegral <$> peek pktSzPtr)
                        <*> newForeignPtr packetMem (c'netcode_server_free_packet s (castPtr packetMem))