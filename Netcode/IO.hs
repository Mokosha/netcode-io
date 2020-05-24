{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Netcode.IO (
      initialize
    , terminate

    , Address
    , parseAddress
    , addressToString
    , addressEqual

    , Packet(..)
    , maximumPacketSize

    , ClientConfig
    , ClientStateChangeCallback
    , SendPacketOverride
    , ReceivePacketOverride
    , defaultClientConfig
    , setClientStateChangeCallback, clearClientStateChangeCallback
    , setSendReceiveOverrides, clearSendReceiveOverrides

    , Client
    , maximumUserDataSize
    , maximumServersPerConnect
    , createClient
    , destroyClient
    , generateClientID
    , connectClient
    , disconnectClient
    , updateClient
    , sendPacketFromClient
    , receivePacketFromServer
    , nextClientPacketSequence
    , getClientPort
    , withClientServerAddress

    , ClientState(..)
    , getClientState
    , isClientDisconnected

    , generateConnectToken

    , ServerConfig
    , defaultServerConfig
    , setProtocolID
    , setPrivateKey

    , Server
    , createServer
    , destroyServer
    , startServer
    , maxNumClients
    , stopServer
    , updateServer
    , isClientConnected
    , maxClientsForServer
    , numConnectedClients
    , serverIsRunning
    , serverIsFull
    , getServerPort
    , sendPacketFromServer
    , disconnectClientFromServer
    , receivePacketFromClient

    , sleep

    , LogLevel(..), logLevel
) where
--------------------------------------------------------------------------------

import Bindings.Netcode.IO

import Control.Applicative   (liftA2)
import Data.Data             (Data)
import Data.Typeable         (Typeable)
import Data.Word             (Word8, Word16, Word64)
import Foreign.C.String      (withCString, peekCString)
import Foreign.C.Types       (CDouble(..), CInt)
import Foreign.Concurrent    (newForeignPtr)
import Foreign.ForeignPtr    ( ForeignPtr, newForeignPtr_, mallocForeignPtr
                             , mallocForeignPtrBytes, withForeignPtr
                             )
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (allocaArray, pokeArray)
import Foreign.Ptr           ( Ptr, nullPtr, castPtr
                             , FunPtr, nullFunPtr, freeHaskellFunPtr
                             )
import Foreign.Storable      (peek, poke, sizeOf, pokeElemOff)
import GHC.Generics          (Generic)

--------------------------------------------------------------------------------

freeNullFunPtr :: FunPtr a -> IO ()
freeNullFunPtr x
    | x == nullFunPtr = return ()
    | otherwise       = freeHaskellFunPtr x

--------------------------------------------------------------------------------

-- | Initializes the netcode.io library. Throws an IOException on failure.
initialize :: IO ()
initialize = do
    result <- c'netcode_init
    if result == c'NETCODE_OK
        then return ()
        else fail "Failed to initialize netcode.io"

terminate :: IO ()
terminate = c'netcode_term

newtype Address = Address (ForeignPtr C'netcode_address_t)
    deriving (Show)

parseAddress :: String -> IO Address
parseAddress addrStr = do
    address <- mallocForeignPtr
    retVal <- withForeignPtr address $ \addressPtr ->
                withCString addrStr (`c'netcode_parse_address` addressPtr)
    if retVal == c'NETCODE_OK
        then return $ Address address
        else fail "Unable to parse address"

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

data Packet = Packet {
    packetSequenceNumber :: Word64,
    packetSize :: Int,
    packetDataPtr :: ForeignPtr Word8
}

maximumPacketSize :: Num a => a
maximumPacketSize = c'NETCODE_MAX_PACKET_SIZE

generateClientID :: IO Word64
generateClientID = alloca $ \idPtr -> do
    c'netcode_random_bytes (castPtr idPtr) $
        fromIntegral $ sizeOf (undefined :: Word64)
    peek idPtr

newtype ConnectToken = ConnectToken (ForeignPtr Word8)

-- | Creates a connect token for the given client (by clientID) with the list
-- of associated addresses. Throws an IOException on failure.
generateConnectToken :: [(String, String)] -- ^ Public and internal servers
                     -> Int                -- ^ Token expiration in seconds
                     -> Int                -- ^ Token timeout in seconds
                     -> Word64             -- ^ Client ID
                     -> Word64             -- ^ Protocol ID
                     -> [Word8]            -- ^ Private key
                     -> [Word8]            -- ^ User data
                     -> IO ConnectToken
generateConnectToken addrs expiry timeout clientID protocolID privateKey userData =
    allocaArray (length addrs) $ \externalAddrs ->
    allocaArray (length addrs) $ \internalAddrs ->
        let writeAddrsAndGo _ [] = continueWithAddrsWritten
            writeAddrsAndGo i ((s1, s2) : rest) = withCString s1 $ \cs1 -> do
                pokeElemOff externalAddrs i cs1
                withCString s2 $ \cs2 -> do
                    pokeElemOff internalAddrs i cs2
                    writeAddrsAndGo (i + 1) rest

            continueWithAddrsWritten = 
                allocaArray c'NETCODE_USER_DATA_BYTES $ \userDataBytes ->
                allocaArray c'NETCODE_KEY_BYTES $ \privateKeyBytes -> do
                    connectTokenPtr <- mallocForeignPtrBytes c'NETCODE_CONNECT_TOKEN_BYTES
                    pokeArray privateKeyBytes (take c'NETCODE_KEY_BYTES       $ privateKey <> repeat 0)
                    pokeArray userDataBytes   (take c'NETCODE_USER_DATA_BYTES $ userData   <> repeat 0)
                    result <- withForeignPtr connectTokenPtr $ \connectTokenBytes ->
                        c'netcode_generate_connect_token (fromIntegral $ length addrs)
                                                         externalAddrs
                                                         internalAddrs
                                                         (fromIntegral expiry)
                                                         (fromIntegral timeout)
                                                         clientID
                                                         protocolID
                                                         privateKeyBytes
                                                         userDataBytes
                                                         connectTokenBytes
                    if result == c'NETCODE_ERROR
                        then fail "Error generating connect token"
                        else return $ ConnectToken connectTokenPtr
         in writeAddrsAndGo 0 addrs

data ClientState
    = ClientState'ConnectTokenExpired
    | ClientState'InvalidConnectToken
    | ClientState'ConnectionTimedOut
    | ClientState'ConnectionResponseTimedOut
    | ClientState'ConnectionRequestTimedOut
    | ClientState'ConnectionDenied
    | ClientState'Disconnected
    | ClientState'SendingConnectionRequest
    | ClientState'SendingConnectionResponse
    | ClientState'Connected
    deriving (Eq, Ord, Show, Enum, Bounded)

_rawClientState :: ClientState -> CInt
_rawClientState ClientState'ConnectTokenExpired        = c'NETCODE_CLIENT_STATE_CONNECT_TOKEN_EXPIRED
_rawClientState ClientState'InvalidConnectToken        = c'NETCODE_CLIENT_STATE_INVALID_CONNECT_TOKEN
_rawClientState ClientState'ConnectionTimedOut         = c'NETCODE_CLIENT_STATE_CONNECTION_TIMED_OUT
_rawClientState ClientState'ConnectionResponseTimedOut = c'NETCODE_CLIENT_STATE_CONNECTION_RESPONSE_TIMED_OUT
_rawClientState ClientState'ConnectionRequestTimedOut  = c'NETCODE_CLIENT_STATE_CONNECTION_REQUEST_TIMED_OUT
_rawClientState ClientState'ConnectionDenied           = c'NETCODE_CLIENT_STATE_CONNECTION_DENIED
_rawClientState ClientState'Disconnected               = c'NETCODE_CLIENT_STATE_DISCONNECTED
_rawClientState ClientState'SendingConnectionRequest   = c'NETCODE_CLIENT_STATE_SENDING_CONNECTION_REQUEST
_rawClientState ClientState'SendingConnectionResponse  = c'NETCODE_CLIENT_STATE_SENDING_CONNECTION_RESPONSE
_rawClientState ClientState'Connected                  = c'NETCODE_CLIENT_STATE_CONNECTED

typedClientState :: CInt -> ClientState
typedClientState raw
  | raw == c'NETCODE_CLIENT_STATE_CONNECT_TOKEN_EXPIRED         = ClientState'ConnectTokenExpired
  | raw == c'NETCODE_CLIENT_STATE_INVALID_CONNECT_TOKEN         = ClientState'InvalidConnectToken
  | raw == c'NETCODE_CLIENT_STATE_CONNECTION_TIMED_OUT          = ClientState'ConnectionTimedOut
  | raw == c'NETCODE_CLIENT_STATE_CONNECTION_RESPONSE_TIMED_OUT = ClientState'ConnectionResponseTimedOut
  | raw == c'NETCODE_CLIENT_STATE_CONNECTION_REQUEST_TIMED_OUT  = ClientState'ConnectionRequestTimedOut
  | raw == c'NETCODE_CLIENT_STATE_CONNECTION_DENIED             = ClientState'ConnectionDenied
  | raw == c'NETCODE_CLIENT_STATE_DISCONNECTED                  = ClientState'Disconnected
  | raw == c'NETCODE_CLIENT_STATE_SENDING_CONNECTION_REQUEST    = ClientState'SendingConnectionRequest
  | raw == c'NETCODE_CLIENT_STATE_SENDING_CONNECTION_RESPONSE   = ClientState'SendingConnectionResponse
  | raw == c'NETCODE_CLIENT_STATE_CONNECTED                     = ClientState'Connected
  | otherwise = error "Unrecognized client state value"

data Client = Client 
  { clientPtr :: Ptr C'netcode_client_t
  , clientCallbacks :: ClientCallbacks
  } deriving (Show)

data ClientCallbacks = ClientCallbacks
  { clientStateChange           :: C'state_change_callback_t
  , clientSendPacketOverride    :: C'send_packet_override_t
  , clientReceivePacketOverride :: C'receive_packet_override_t
  } deriving (Show)

defaultClientCallbacks :: ClientCallbacks
defaultClientCallbacks = ClientCallbacks nullFunPtr nullFunPtr nullFunPtr

newtype ClientConfig = ClientConfig
  (   Ptr C'netcode_client_config_t
   -> ClientCallbacks
   -> IO (Ptr C'netcode_client_config_t, ClientCallbacks)
  )

maximumUserDataSize :: Num a => a
maximumUserDataSize = c'NETCODE_USER_DATA_BYTES

maximumServersPerConnect :: Num a => a
maximumServersPerConnect = c'NETCODE_MAX_SERVERS_PER_CONNECT

defaultClientConfig :: ClientConfig
defaultClientConfig = ClientConfig $ \clientConfig cbs -> do
    c'netcode_default_client_config clientConfig
    return (clientConfig, cbs)

type ClientStateChangeCallback = ClientState -> ClientState -> IO ()

setClientStateChangeCallback :: ClientStateChangeCallback
                             -> ClientConfig -> ClientConfig
setClientStateChangeCallback cb (ClientConfig mkConfig) =
    ClientConfig $ \configPtr' callbacks' -> do
        (configPtr, callbacks) <- mkConfig configPtr' callbacks'
        freeNullFunPtr $ clientStateChange callbacks
        fPtr <- mk'state_change_callback_t $ \_ oldSt newSt ->
            cb (typedClientState oldSt) (typedClientState newSt)
        config <- peek configPtr
        poke configPtr $
            config { c'netcode_client_config_t'state_change_callback = fPtr }
        return (configPtr, callbacks { clientStateChange = fPtr })

clearClientStateChangeCallback :: ClientConfig -> ClientConfig
clearClientStateChangeCallback (ClientConfig mkConfig) =
    ClientConfig $ \configPtr' callbacks' -> do
        (configPtr, callbacks) <- mkConfig configPtr' callbacks'
        freeNullFunPtr $ clientStateChange callbacks
        config <- peek configPtr
        poke configPtr $
            config { c'netcode_client_config_t'state_change_callback = nullFunPtr }
        return (configPtr, callbacks { clientStateChange = nullFunPtr })

type SendPacketOverride = Address -> Ptr Word8 -> CInt -> IO ()
type ReceivePacketOverride = Address -> Ptr Word8 -> CInt -> IO CInt

setSendReceiveOverrides :: SendPacketOverride
                        -> ReceivePacketOverride
                        -> ClientConfig -> ClientConfig
setSendReceiveOverrides sendFn recvFn (ClientConfig mkConfig) =
    ClientConfig $ \configPtr' callbacks' -> do
        (configPtr, callbacks) <- mkConfig configPtr' callbacks'
        freeNullFunPtr $ clientSendPacketOverride callbacks
        freeNullFunPtr $ clientReceivePacketOverride callbacks
        config <- peek configPtr
        sendOverride <- mk'send_packet_override_t $ \_ aptr pkt pktSize -> do
            addr <- Address <$> newForeignPtr_ aptr
            sendFn addr pkt pktSize
        recvOverride <- mk'receive_packet_override_t $
            \_ aptr pkt pktSize -> do
                addr <- Address <$> newForeignPtr_ aptr
                recvFn addr pkt pktSize
        poke configPtr $ config
            { c'netcode_client_config_t'send_packet_override = sendOverride
            , c'netcode_client_config_t'receive_packet_override = recvOverride
            , c'netcode_client_config_t'override_send_and_receive = 1
            }
        let newcbs = callbacks
              { clientSendPacketOverride = sendOverride
              , clientReceivePacketOverride = recvOverride
              }
        return (configPtr, newcbs)

clearSendReceiveOverrides :: ClientConfig -> ClientConfig
clearSendReceiveOverrides (ClientConfig mkConfig) =
    ClientConfig $ \configPtr' callbacks' -> do
        (configPtr, callbacks) <- mkConfig configPtr' callbacks'
        freeNullFunPtr $ clientSendPacketOverride callbacks
        freeNullFunPtr $ clientReceivePacketOverride callbacks
        config <- peek configPtr
        poke configPtr $ config
            { c'netcode_client_config_t'send_packet_override = nullFunPtr
            , c'netcode_client_config_t'receive_packet_override = nullFunPtr
            , c'netcode_client_config_t'override_send_and_receive = 0
            }
        let newcbs = callbacks
              { clientSendPacketOverride = nullFunPtr
              , clientReceivePacketOverride = nullFunPtr
              }
        return (configPtr, newcbs)

-- | Creates a client at the given address using the provided config. Throws an
-- IOException on failure.
createClient :: String -> ClientConfig -> Double -> IO Client
createClient s (ClientConfig mkConfig) time = alloca $ \clientConfig -> do
    (config, callbacks) <- mkConfig clientConfig defaultClientCallbacks
    ptr <- withCString s $ \cs ->
        c'netcode_client_create cs config (CDouble time)
    if ptr == nullPtr
        then fail "Failed to create client!"
        else return (Client ptr callbacks)

destroyClient :: Client -> IO ()
destroyClient (Client c cbs) = do
    c'netcode_client_destroy c
    freeNullFunPtr $ clientStateChange cbs
    freeNullFunPtr $ clientSendPacketOverride cbs
    freeNullFunPtr $ clientReceivePacketOverride cbs

connectClient :: Client -> ConnectToken -> IO ()
connectClient (Client c _) (ConnectToken ctPtr) =
    withForeignPtr ctPtr (c'netcode_client_connect c)

disconnectClient :: Client -> IO ()
disconnectClient (Client c _) = c'netcode_client_disconnect c

updateClient :: Client -> Double -> IO ()
updateClient (Client c _) = c'netcode_client_update c . CDouble

getClientState :: Client -> IO ClientState
getClientState (Client c _) = typedClientState <$> c'netcode_client_state c

isClientDisconnected :: Client -> IO Bool
isClientDisconnected (Client c _) =
    (<= c'NETCODE_CLIENT_STATE_DISCONNECTED) <$> c'netcode_client_state c

nextClientPacketSequence :: Client -> IO Word64
nextClientPacketSequence (Client c _) = c'netcode_client_next_packet_sequence c

sendPacketFromClient :: Client -> Int -> Ptr Word8 -> IO ()
sendPacketFromClient (Client c _) pktSz pktMem =
    let pktSize = min c'NETCODE_MAX_PACKET_SIZE (fromIntegral pktSz)
     in do
        if pktSz > c'NETCODE_MAX_PACKET_SIZE
            then putStrLn $ 
                "WARNING: Sending packet that's too large: " <> show pktSz
            else return ()
        c'netcode_client_send_packet c pktMem pktSize

receivePacketFromServer :: Client -> IO (Maybe Packet)
receivePacketFromServer (Client c _) =
    alloca $ \sequenceNumPtr ->
    alloca $ \pktSzPtr -> do
        packetMem <- c'netcode_client_receive_packet c pktSzPtr sequenceNumPtr
        let finalizer = c'netcode_client_free_packet c (castPtr packetMem)
        if packetMem == nullPtr
            then return Nothing
            else fmap Just $
                Packet <$> peek sequenceNumPtr
                       <*> (fromIntegral <$> peek pktSzPtr)
                       <*> newForeignPtr packetMem finalizer

getClientPort :: Client -> IO Word16
getClientPort (Client c _) = c'netcode_client_get_port c

-- Note, the address here shouldn't outlive the client.
withClientServerAddress :: Client -> (Address -> IO a) -> IO a
withClientServerAddress (Client c _) fn =
    Address <$> newForeignPtr_ (c'netcode_client_server_address c) >>= fn

newtype Server = Server (Ptr C'netcode_server_t) deriving (Show)
newtype ServerConfig = 
    ServerConfig (Ptr C'netcode_server_config_t ->
                    IO (Ptr C'netcode_server_config_t))

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig $ \serverConfig -> do
    c'netcode_default_server_config serverConfig
    return serverConfig

setProtocolID :: Word64 -> ServerConfig -> ServerConfig
setProtocolID protocolID (ServerConfig mkServerPtr) =
    ServerConfig $ \serverConfig -> do
        configPtr <- mkServerPtr serverConfig
        config <- peek configPtr
        poke configPtr $
            config { c'netcode_server_config_t'protocol_id = protocolID }
        return configPtr

setPrivateKey :: [Word8] -> ServerConfig -> ServerConfig
setPrivateKey key (ServerConfig mkServerPtr) =
    ServerConfig $ \serverConfig -> do
        configPtr <- mkServerPtr serverConfig
        config <- peek configPtr
        poke configPtr $    
            config {
                c'netcode_server_config_t'private_key = 
                    take c'NETCODE_KEY_BYTES (key <> repeat 0)
            }
        return configPtr

-- | Creates a server at the given address using the provided config. Throws an
-- IOException on failure.
createServer :: String -> ServerConfig -> Double -> IO Server
createServer s (ServerConfig mkConfig) time = alloca $ \serverConfig -> do
    config <- mkConfig serverConfig
    serverPtr <- withCString s (\cs -> c'netcode_server_create cs config (CDouble time))
    if serverPtr == nullPtr
        then fail "Failed to create server!"
        else return (Server serverPtr)

-- | Starts the server and specifies the maximum number of clients that can
-- connect.
startServer :: Server -> Int -> IO ()
startServer (Server s) = c'netcode_server_start s . fromIntegral

maxNumClients :: Num a => a
maxNumClients = c'NETCODE_MAX_CLIENTS

-- | Stops the server.
stopServer :: Server -> IO ()
stopServer (Server s) = c'netcode_server_stop s

destroyServer :: Server -> IO ()
destroyServer (Server s) = c'netcode_server_destroy s        

updateServer :: Server -> Double -> IO ()
updateServer (Server s) = c'netcode_server_update s . CDouble

isClientConnected :: Server -> Int -> IO Bool
isClientConnected (Server s) =
    fmap (/= 0) . c'netcode_server_client_connected s . fromIntegral

maxClientsForServer :: Server -> IO Int
maxClientsForServer (Server s) =
    fromIntegral <$> c'netcode_server_max_clients s

numConnectedClients :: Server -> IO Int
numConnectedClients (Server s) =
    fromIntegral <$> c'netcode_server_num_connected_clients s

serverIsRunning :: Server -> IO Bool
serverIsRunning (Server s) = (/= 0) <$> c'netcode_server_running s

serverIsFull :: Server -> IO Bool
serverIsFull (Server s) =
    liftA2 (==) (c'netcode_server_num_connected_clients s)
                (c'netcode_server_max_clients s)

getServerPort :: Server -> IO Word16
getServerPort (Server s) = c'netcode_server_get_port s

disconnectClientFromServer :: Server -> Int -> IO ()
disconnectClientFromServer (Server s) =
    c'netcode_server_disconnect_client s . fromIntegral

sendPacketFromServer :: Server -> Int -> Int -> Ptr Word8 -> IO ()
sendPacketFromServer (Server s) clientIdx pktSz pktMem =
    let pktSize = min c'NETCODE_MAX_PACKET_SIZE (fromIntegral pktSz)
     in do
        if pktSz > c'NETCODE_MAX_PACKET_SIZE
            then putStrLn $ "WARNING: Sending packet that's too large: " <> show pktSz
            else return ()
        c'netcode_server_send_packet s (fromIntegral clientIdx) pktMem pktSize

receivePacketFromClient :: Server -> Int -> IO (Maybe Packet)
receivePacketFromClient (Server s) clientIdx =
    alloca $ \sequenceNumPtr ->
    alloca $ \pktSzPtr -> do
        packetMem <- c'netcode_server_receive_packet s (fromIntegral clientIdx) pktSzPtr sequenceNumPtr
        if packetMem == nullPtr
            then return Nothing
            else fmap Just $
                Packet <$> peek sequenceNumPtr
                       <*> (fromIntegral <$> peek pktSzPtr)
                       <*> newForeignPtr packetMem (c'netcode_server_free_packet s (castPtr packetMem))

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
