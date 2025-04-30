module WebSocket (
    Connection,
    startClient,
    sendMessage,
    receiveMessage,
) where

import Control.Concurrent.Chan

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe
import Data.Function
import Data.Unique
import Data.Word

import Erebos.Network

import Foreign.Marshal.Alloc
import Foreign.Ptr

import GHC.Wasm.Prim

import JavaScript qualified as JS


data Connection = Connection
    { connUnique :: Unique
    , connAddress :: String
    , connJS :: JSVal
    , connInQueue :: Chan ByteString
    }

instance Eq Connection where
    (==) = (==) `on` connUnique

instance Ord Connection where
    compare = compare `on` connUnique

instance Show Connection where
    show = connAddress

instance PeerAddressType Connection where
    sendBytesToAddress = sendMessage

startClient :: String -> Int -> String -> (Connection -> IO ()) -> IO ()
startClient addr port path fun = do
    connUnique <- newUnique
    let connAddress = "ws://" <> addr <> ":" <> show port <> "/" <> path
    connJS <- js_initWebSocket (toJSString connAddress)
    connInQueue <- newChan
    let conn = Connection {..}

    JS.addEventListener connJS "open" $ \_ -> do
        fun conn

    JS.addEventListener connJS "message" $ \ev -> do
        bytes <- js_get_data ev
        len <- js_get_byteLength bytes
        ptr <- mallocBytes len
        js_copyBytes ptr bytes
        bs <- unsafePackCStringFinalizer ptr len (free ptr)
        writeChan connInQueue bs


sendMessage :: Connection -> ByteString -> IO ()
sendMessage Connection {..} bs = do
    unsafeUseAsCStringLen bs $ \( ptr, len ) -> do
        js_send connJS (castPtr ptr) len

receiveMessage :: Connection -> IO ByteString
receiveMessage Connection {..} = do
    readChan connInQueue


foreign import javascript unsafe "const ws = new WebSocket($1); ws.binaryType = 'arraybuffer'; return ws"
    js_initWebSocket :: JSString -> IO JSVal

foreign import javascript unsafe "$1.send(new Uint8Array(globalThis.wasi_memory.buffer, $2, $3))"
    js_send :: JSVal -> Ptr Word8 -> Int -> IO ()

foreign import javascript unsafe "$1.data"
    js_get_data :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.byteLength"
    js_get_byteLength :: JSVal -> IO Int

foreign import javascript unsafe "new Uint8Array(globalThis.wasi_memory.buffer, $1, $2.byteLength).set(new Uint8Array($2))"
    js_copyBytes :: Ptr Word8 -> JSVal -> IO ()
