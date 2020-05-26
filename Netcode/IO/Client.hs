module Netcode.IO.Client (
      ClientConfig
    , defaultClientConfig
    , ClientStateChangeCallback
    , setClientStateChangeCallback, clearClientStateChangeCallback
    , setClientSendReceiveOverrides, clearClientSendReceiveOverrides

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

    , ConnectToken
    , generateConnectToken
) where

--------------------------------------------------------------------------------

import Control.Monad         (when)
import Data.Word             (Word8, Word16, Word64)
import Foreign.C.String      (withCString)
import Foreign.C.Types       (CDouble(..), CInt)
import Foreign.Concurrent    (newForeignPtr)
import Foreign.ForeignPtr    ( ForeignPtr, newForeignPtr_, mallocForeignPtrBytes
                             , withForeignPtr
                             )
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, pokeArray)
import Foreign.Ptr           ( Ptr, nullPtr, castPtr
                             , FunPtr, nullFunPtr, freeHaskellFunPtr
                             )
import Foreign.Storable      (peek, poke, sizeOf, pokeElemOff)

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

mkClientStateChangeCallback :: ClientStateChangeCallback
                            -> IO C'state_change_callback_t
mkClientStateChangeCallback cb = mk'state_change_callback_t $ \_ oldSt newSt ->
    cb (typedClientState oldSt) (typedClientState newSt)

setClientStateChangeCallback :: ClientStateChangeCallback
                             -> ClientConfig -> ClientConfig
setClientStateChangeCallback cb (ClientConfig mkConfig) =
    ClientConfig $ \configPtr' callbacks' -> do
        (configPtr, callbacks) <- mkConfig configPtr' callbacks'
        freeNullFunPtr $ clientStateChange callbacks
        fPtr <- mkClientStateChangeCallback cb
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

setClientSendReceiveOverrides :: SendPacketOverride
                              -> ReceivePacketOverride
                              -> ClientConfig -> ClientConfig
setClientSendReceiveOverrides sendFn recvFn (ClientConfig mkConfig) =
    ClientConfig $ \configPtr' callbacks' -> do
        (configPtr, callbacks) <- mkConfig configPtr' callbacks'
        freeNullFunPtr $ clientSendPacketOverride callbacks
        freeNullFunPtr $ clientReceivePacketOverride callbacks
        config <- peek configPtr
        sendOverride <- mkSendPacketOverride sendFn
        recvOverride <- mkReceivePacketOverride recvFn
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

clearClientSendReceiveOverrides :: ClientConfig -> ClientConfig
clearClientSendReceiveOverrides (ClientConfig mkConfig) =
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
    when (ptr == nullPtr) $ fail "Failed to create client!"
    return (Client ptr callbacks)

destroyClient :: Client -> IO ()
destroyClient (Client c cbs) = do
    c'netcode_client_destroy c
    freeNullFunPtr $ clientStateChange cbs
    freeNullFunPtr $ clientSendPacketOverride cbs
    freeNullFunPtr $ clientReceivePacketOverride cbs


generateClientID :: IO Word64
generateClientID = alloca $ \idPtr -> do
    c'netcode_random_bytes (castPtr idPtr) $
        fromIntegral $ sizeOf (undefined :: Word64)
    peek idPtr

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
sendPacketFromClient (Client c _) pktSz pktMem = do
    let pktSize = min c'NETCODE_MAX_PACKET_SIZE (fromIntegral pktSz)
    when (pktSz > c'NETCODE_MAX_PACKET_SIZE) $ putStrLn $
        "WARNING: Sending packet that's too large: " <> show pktSz
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
withClientServerAddress (Client c _) fn = do
    aptr <- c'netcode_client_server_address c
    Address <$> newForeignPtr_ aptr >>= fn

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
                    when (result == c'NETCODE_ERROR) $ fail "Error generating connect token"
                    return $ ConnectToken connectTokenPtr
          in writeAddrsAndGo 0 addrs
    