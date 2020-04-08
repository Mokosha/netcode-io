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
    , defaultClientConfig

    , Client
    , createClient
    , destroyClient
    , generateClientID
    , connectClient
    , updateClient
    , sendPacketFromClient
    , receivePacketFromServer

    , ClientState(..)
    , getClientState

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
    , sendPacketFromServer
    , disconnectClientFromServer
    , receivePacketFromClient

    , sleep

    , LogLevel(..), logLevel
) where
--------------------------------------------------------------------------------

import Bindings.Netcode.IO

import Data.Data             (Data)
import Data.Typeable         (Typeable)
import Data.Word             (Word8, Word64)
import Foreign.C.String      (withCString, peekCString)
import Foreign.C.Types       (CDouble(..))
import Foreign.Concurrent    (newForeignPtr)
import Foreign.ForeignPtr    ( ForeignPtr, mallocForeignPtr, mallocForeignPtrBytes
                             , withForeignPtr
                             )
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (allocaArray, pokeArray)
import Foreign.Ptr           (Ptr, nullPtr, castPtr)
import Foreign.Storable      (peek, poke, sizeOf, pokeElemOff)
import GHC.Generics          (Generic)

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

newtype Client = Client (Ptr C'netcode_client_t)
newtype ClientConfig =
    ClientConfig (Ptr C'netcode_client_config_t ->
                    IO (Ptr C'netcode_client_config_t))

defaultClientConfig :: ClientConfig
defaultClientConfig = ClientConfig $ \clientConfig -> do
    c'netcode_default_client_config clientConfig
    return clientConfig

-- | Creates a client at the given address using the provided config. Throws an
-- IOException on failure.
createClient :: String -> ClientConfig -> Double -> IO Client
createClient s (ClientConfig mkConfig) time = alloca $ \clientConfig -> do
    config <- mkConfig clientConfig
    clientPtr <- withCString s $ \cs ->
        c'netcode_client_create cs config (CDouble time)
    if clientPtr == nullPtr
        then fail "Failed to create client!"
        else return (Client clientPtr)

destroyClient :: Client -> IO ()
destroyClient (Client c) = c'netcode_client_destroy c

connectClient :: Client -> ConnectToken -> IO ()
connectClient (Client c) (ConnectToken ctPtr) =
    withForeignPtr ctPtr (c'netcode_client_connect c)

updateClient :: Client -> Double -> IO ()
updateClient (Client c) = c'netcode_client_update c . CDouble

getClientState :: Client -> IO ClientState
getClientState (Client c) = do
    stateNum <- c'netcode_client_state c
    let mapping = 
            [ (c'NETCODE_CLIENT_STATE_CONNECT_TOKEN_EXPIRED         , ClientState'ConnectTokenExpired)
            , (c'NETCODE_CLIENT_STATE_INVALID_CONNECT_TOKEN         , ClientState'InvalidConnectToken)
            , (c'NETCODE_CLIENT_STATE_CONNECTION_TIMED_OUT          , ClientState'ConnectionTimedOut)
            , (c'NETCODE_CLIENT_STATE_CONNECTION_RESPONSE_TIMED_OUT , ClientState'ConnectionResponseTimedOut)
            , (c'NETCODE_CLIENT_STATE_CONNECTION_REQUEST_TIMED_OUT  , ClientState'ConnectionRequestTimedOut)
            , (c'NETCODE_CLIENT_STATE_CONNECTION_DENIED             , ClientState'ConnectionDenied)
            , (c'NETCODE_CLIENT_STATE_DISCONNECTED                  , ClientState'Disconnected)
            , (c'NETCODE_CLIENT_STATE_SENDING_CONNECTION_REQUEST    , ClientState'SendingConnectionRequest)
            , (c'NETCODE_CLIENT_STATE_SENDING_CONNECTION_RESPONSE   , ClientState'SendingConnectionResponse)
            , (c'NETCODE_CLIENT_STATE_CONNECTED                     , ClientState'Connected)
            ]
    case filter ((== stateNum) . fst) mapping of
        [] -> fail "Unrecognized client state!"
        ((_, r) : _) -> return r

sendPacketFromClient :: Client -> Int -> Ptr Word8 -> IO ()
sendPacketFromClient (Client c) pktSz pktMem =
    let pktSize = min c'NETCODE_MAX_PACKET_SIZE (fromIntegral pktSz)
     in do
        if pktSz > c'NETCODE_MAX_PACKET_SIZE
            then putStrLn $ "WARNING: Sending packet that's too large: " <> show pktSz
            else return ()
        c'netcode_client_send_packet c pktMem pktSize

receivePacketFromServer :: Client -> IO (Maybe Packet)
receivePacketFromServer (Client c) =
    alloca $ \sequenceNumPtr ->
    alloca $ \pktSzPtr -> do
        packetMem <- c'netcode_client_receive_packet c pktSzPtr sequenceNumPtr
        if packetMem == nullPtr
            then return Nothing
            else fmap Just $
                Packet <$> peek sequenceNumPtr
                       <*> (fromIntegral <$> peek pktSzPtr)
                       <*> newForeignPtr packetMem (c'netcode_client_free_packet c (castPtr packetMem))

newtype Server = Server (Ptr C'netcode_server_t)
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
