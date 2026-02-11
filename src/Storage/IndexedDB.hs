module Storage.IndexedDB (
    indexedDBStorage,
) where

import Control.Concurrent.MVar
import Control.Monad

import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe
import Data.Function
import Data.Maybe
import Data.UUID.Types qualified as U
import Data.Word

import Erebos.Object
import Erebos.Storable
import Erebos.Storage.Backend

import Foreign.Marshal.Alloc
import Foreign.Ptr

import GHC.Wasm.Prim

import JavaScript qualified as JS
import Storage.WatchList


data IndexedDBStorage = IndexedDBStorage
    { bsName :: String
    , bsDatabase :: JSVal
    , bsWatchers :: MVar WatchList
    , idbReadLock :: MVar ()
    , idbReadValue :: MVar (Maybe BL.ByteString)
    , idbReadHandler :: JSVal
    }

instance Eq IndexedDBStorage where
    (==) = (==) `on` bsName

instance Show IndexedDBStorage where
    show IndexedDBStorage {..} = "IndexedDB@" <> bsName

instance StorageBackend IndexedDBStorage where
    backendLoadBytes IndexedDBStorage {..} dgst = do
        withMVar idbReadLock $ \_ -> do
            unsafeUseAsCStringLen (refDigestBytes dgst) $ \( dgstPtr, dgstLen ) -> do
                js_db_get bsDatabase (toJSString "objects") (castPtr dgstPtr) dgstLen idbReadHandler
                takeMVar idbReadValue

    backendStoreBytes IndexedDBStorage {..} dgst raw = do
        unsafeUseAsCStringLen (refDigestBytes dgst) $ \( dgstPtr, dgstLen ) -> do
            unsafeUseAsCStringLen (BL.toStrict raw) $ \( ptr, len ) -> do
                js_db_put bsDatabase (toJSString "objects") (castPtr dgstPtr) dgstLen (castPtr ptr) len


    backendLoadHeads IndexedDBStorage {..} tid = do
        mvar <- newEmptyMVar
        handler <- JS.asEventListener $ \ev -> do
            res <- js_get_targetResult ev
            count <- js_get_length res
            keys <- forM [ 0 .. count - 1 ] $ \i -> do
                jkey <- js_array_index res i
                len <- js_get_byteLength jkey
                ptr <- mallocBytes len
                js_copyBytes ptr jkey
                unsafePackCStringFinalizer ptr len (free ptr)

            pairs <- fmap catMaybes $ forM (filter ((BC.pack (U.toString (toUUID tid)) ==) . BC.take 36) keys) $ \bkey -> do
                case U.fromString $ BC.unpack $ BC.drop 37 bkey of
                    Just uhid -> do
                        unsafeUseAsCStringLen bkey $ \( bkeyPtr, bkeyLen ) -> do
                            js_db_get bsDatabase (toJSString "heads") (castPtr bkeyPtr) bkeyLen idbReadHandler
                        dgst <- takeMVar idbReadValue
                        return $ ( fromUUID uhid, ) <$> (readRefDigest . BL.toStrict =<< dgst)
                    Nothing -> do
                        return Nothing
            putMVar mvar pairs
        withMVar idbReadLock $ \_ -> do
            js_db_get_all_keys bsDatabase (toJSString "heads") handler
            takeMVar mvar

    backendLoadHead IndexedDBStorage {..} tid hid = do
        withMVar idbReadLock $ \_ -> do
            mvar <- newEmptyMVar
            handler <- JS.asEventListener $ \ev -> do
                res <- js_get_targetResult ev
                len <- js_get_byteLength res
                ptr <- mallocBytes len
                js_copyBytes ptr res
                bs <- unsafePackCStringFinalizer ptr len (free ptr)
                putMVar mvar $ readRefDigest bs
            unsafeUseAsCStringLen (BC.pack $ U.toString (toUUID tid) ++ "_" ++ U.toString (toUUID hid)) $ \( keyPtr, keyLen ) -> do
                js_db_get bsDatabase (toJSString "heads") (castPtr keyPtr) keyLen handler
            takeMVar mvar

    backendStoreHead IndexedDBStorage {..} tid hid dgst =
        withMVar idbReadLock $ \_ -> do
            unsafeUseAsCStringLen (BC.pack $ U.toString (toUUID tid) ++ "_" ++ U.toString (toUUID hid)) $ \( keyPtr, keyLen ) -> do
                unsafeUseAsCStringLen (showRefDigest dgst) $ \( ptr, len ) -> do
                    js_db_put bsDatabase (toJSString "heads") (castPtr keyPtr) keyLen (castPtr ptr) len

    backendReplaceHead IndexedDBStorage {..} tid hid expected new = do
        res <- withMVar idbReadLock $ \_ -> do
            mvar <- newEmptyMVar
            handler <- JS.asEventListener $ \ev -> do
                res <- js_get_targetResult ev
                len <- js_get_byteLength res
                ptr <- mallocBytes len
                js_copyBytes ptr res
                bs <- unsafePackCStringFinalizer ptr len (free ptr)
                putMVar mvar $ readRefDigest bs
            unsafeUseAsCStringLen (BC.pack $ U.toString (toUUID tid) ++ "_" ++ U.toString (toUUID hid)) $ \( keyPtr, keyLen ) ->
                unsafeUseAsCStringLen (showRefDigest new) $ \( nptr, nlen ) -> do
                    js_db_get bsDatabase (toJSString "heads") (castPtr keyPtr) keyLen handler
                    takeMVar mvar >>= \case
                        Just dgst
                            | dgst == expected
                            -> do
                                js_db_put bsDatabase (toJSString "heads") (castPtr keyPtr) keyLen (castPtr nptr) nlen
                                ws <- map wlFun . filter ((==(tid, hid)) . wlHead) . wlList <$> readMVar bsWatchers
                                return $ Right ( new, ws )
                            | otherwise -> do
                                return $ Left (Just dgst)
                        Nothing -> do
                            return $ Left Nothing
        case res of
            Right ( dgst, ws ) -> mapM_ ($ dgst) ws >> return (Right dgst)
            Left x -> return $ Left x

    backendWatchHead IndexedDBStorage {..} tid hid cb = do
        modifyMVar bsWatchers $ return . watchListAdd tid hid cb

    backendUnwatchHead IndexedDBStorage {..} wid = do
        modifyMVar_ bsWatchers $ return . watchListDel wid


    backendListKeys IndexedDBStorage {..} = _
    backendLoadKey IndexedDBStorage {..} dgst = do
        mvar <- newEmptyMVar
        handler <- JS.asEventListener $ \ev -> do
            res <- js_get_targetResult ev
            len <- js_get_byteLength res
            ptr <- mallocBytes len
            js_copyBytes ptr res
            bs <- unsafePackCStringFinalizer ptr len (free ptr)
            putMVar mvar $ Just bs
        unsafeUseAsCStringLen (refDigestBytes dgst) $ \( dgstPtr, dgstLen ) -> do
            js_db_get bsDatabase (toJSString "keys") (castPtr dgstPtr) dgstLen handler
        takeMVar mvar

    backendStoreKey IndexedDBStorage {..} dgst key = do
        unsafeUseAsCStringLen (refDigestBytes dgst) $ \( dgstPtr, dgstLen ) -> do
            unsafeUseAsCStringLen key $ \( ptr, len ) -> do
                js_db_put bsDatabase (toJSString "keys") (castPtr dgstPtr) dgstLen (castPtr ptr) len

    backendRemoveKey IndexedDBStorage {..} dgst = do
        js_db_delete bsDatabase (toJSString "keys") (toJSString $ show dgst)


indexedDBStorage :: String -> IO Storage
indexedDBStorage bsName = do
    dbVar <- newEmptyMVar
    handler <- JS.asEventListener $ \db -> do
        putMVar dbVar db
    js_indexedDB_open (toJSString bsName) handler
    bsDatabase <- takeMVar dbVar
    bsWatchers <- newMVar (WatchList startWatchID [])

    idbReadLock <- newMVar ()
    idbReadValue <- newEmptyMVar
    idbReadHandler <- JS.asEventListener $ \ev -> do
        res <- js_get_targetResult ev
        js_get_byteLength res >>= \case
            0 -> putMVar idbReadValue $ Nothing
            len -> do
                ptr <- mallocBytes len
                js_copyBytes ptr res
                bs <- unsafePackCStringFinalizer ptr len (free ptr)
                putMVar idbReadValue $ Just $ BL.fromStrict bs

    newStorage IndexedDBStorage {..}


foreign import javascript unsafe
    "const req = window.indexedDB.open($1); req.onerror = (event) => { console.log(\"Error loading database.\"); }; req.onsuccess = (event) => { $2(req.result); }; req.onupgradeneeded = (event) => { const db = event.target.result; db.createObjectStore(\"objects\"); db.createObjectStore(\"heads\"); db.createObjectStore(\"keys\"); }"
    js_indexedDB_open :: JSString -> JSVal -> IO ()


foreign import javascript unsafe
    "const tr = $1.transaction($2, \"readwrite\"); tr.onerror = (event) => { console.log(\"put transaction error\"); }; const key = new Uint8Array(globalThis.wasi_memory.buffer, $3, $4); const value = new Uint8Array(globalThis.wasi_memory.buffer, $5, $6); tr.objectStore($2).put(value, key); tr.commit();"
    js_db_put :: JSVal -> JSString -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> IO ()

foreign import javascript unsafe
    "const tr = $1.transaction($2, \"readonly\"); tr.onerror = (event) => { console.log(\"get transaction error\"); }; const req = tr.objectStore($2).get(new Uint8Array(globalThis.wasi_memory.buffer, $3, $4)); req.onsuccess = $5; tr.commit();"
    js_db_get :: JSVal -> JSString -> Ptr Word8 -> Int -> JSVal -> IO ()

foreign import javascript unsafe
    "const tr = $1.transaction($2, \"readwrite\"); tr.onerror = (event) => { console.log(\"delete transaction error\"); }; const req = tr.objectStore($2).delete($3); tr.commit();"
    js_db_delete :: JSVal -> JSString -> JSString -> IO ()

foreign import javascript unsafe
    "const tr = $1.transaction($2, \"readonly\"); tr.onerror = (event) => { console.log(\"list transaction error\"); }; const req = tr.objectStore($2).getAllKeys(); req.onsuccess = $3; tr.commit();"
    js_db_get_all_keys :: JSVal -> JSString -> JSVal -> IO ()

foreign import javascript unsafe "$1.target.result"
    js_get_targetResult :: JSVal -> IO JSVal


foreign import javascript unsafe "if (!$1) { return 0; }; return $1.byteLength"
    js_get_byteLength :: JSVal -> IO Int

foreign import javascript unsafe "new Uint8Array(globalThis.wasi_memory.buffer, $1, $2.byteLength).set(new Uint8Array($2))"
    js_copyBytes :: Ptr Word8 -> JSVal -> IO ()

foreign import javascript unsafe "$1.length"
    js_get_length :: JSVal -> IO Int

foreign import javascript unsafe "$1[$2]"
    js_array_index :: JSVal -> Int -> IO JSVal
