module Storage.IndexedDB (
    indexedDBStorage,
) where

import Control.Concurrent.MVar

import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe
import Data.Function
import Data.List
import Data.Maybe
import Data.Word

import Erebos.Object
import Erebos.Storage.Backend
import Erebos.Storage.Head

import Foreign.Marshal.Alloc
import Foreign.Ptr

import GHC.Wasm.Prim

import JavaScript qualified as JS


data IndexedDBStorage = IndexedDBStorage
    { bsName :: String
    , bsDatabase :: JSVal
    , bsHeads :: MVar [ (( HeadTypeID, HeadID ), RefDigest ) ]
    , bsWatchers :: MVar WatchList
    }

instance Eq IndexedDBStorage where
    (==) = (==) `on` bsName

instance Show IndexedDBStorage where
    show IndexedDBStorage {..} = "IndexedDB@" <> bsName

instance StorageBackend IndexedDBStorage where
    backendLoadBytes IndexedDBStorage {..} dgst = do
        mvar <- newEmptyMVar
        handler <- JS.asEventListener $ \ev -> do
            res <- js_get_targetResult ev
            js_get_byteLength res >>= \case
                0 -> putMVar mvar $ Nothing
                len -> do
                    ptr <- mallocBytes len
                    js_copyBytes ptr res
                    bs <- unsafePackCStringFinalizer ptr len (free ptr)
                    putMVar mvar $ Just $ BL.fromStrict bs
        js_db_get bsDatabase (toJSString "objects") (toJSString $ show dgst) handler
        takeMVar mvar

    backendStoreBytes IndexedDBStorage {..} dgst raw = do
        unsafeUseAsCStringLen (BL.toStrict raw) $ \( ptr, len ) -> do
            js_db_put bsDatabase (toJSString "objects") (toJSString $ show dgst) (castPtr ptr) len


    backendLoadHeads IndexedDBStorage {..} tid = do
        let toRes ( ( tid', hid ), dgst )
                | tid' == tid = Just ( hid, dgst )
                | otherwise   = Nothing
        catMaybes . map toRes <$> readMVar bsHeads

    backendLoadHead IndexedDBStorage {..} tid hid =
        lookup (tid, hid) <$> readMVar bsHeads

    backendStoreHead IndexedDBStorage {..} tid hid dgst =
        modifyMVar_ bsHeads $ return . (( ( tid, hid ), dgst ) :)

    backendReplaceHead IndexedDBStorage {..} tid hid expected new = do
        res <- modifyMVar bsHeads $ \hs -> do
            ws <- map wlFun . filter ((==(tid, hid)) . wlHead) . wlList <$> readMVar bsWatchers
            return $ case partition ((==(tid, hid)) . fst) hs of
                ( [] , _ ) -> ( hs, Left Nothing )
                (( _, dgst ) : _, hs' )
                    | dgst == expected -> ((( tid, hid ), new ) : hs', Right ( new, ws ))
                    | otherwise -> ( hs, Left $ Just dgst )
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
        js_db_get bsDatabase (toJSString "keys") (toJSString $ show dgst) handler
        takeMVar mvar

    backendStoreKey IndexedDBStorage {..} dgst key = do
        unsafeUseAsCStringLen key $ \( ptr, len ) -> do
            js_db_put bsDatabase (toJSString "keys") (toJSString $ show dgst) (castPtr ptr) len

    backendRemoveKey IndexedDBStorage {..} dgst = do
        js_db_delete bsDatabase (toJSString "keys") (toJSString $ show dgst)


indexedDBStorage :: String -> IO Storage
indexedDBStorage bsName = do
    dbVar <- newEmptyMVar
    handler <- JS.asEventListener $ \db -> do
        putMVar dbVar db
    js_indexedDB_open (toJSString bsName) handler
    bsDatabase <- takeMVar dbVar
    bsHeads <- newMVar []
    bsWatchers <- newMVar (WatchList startWatchID [])
    newStorage IndexedDBStorage {..}



data WatchList = WatchList
    { wlNext :: WatchID
    , wlList :: [ WatchListItem ]
    }

data WatchListItem = WatchListItem
    { wlID :: WatchID
    , wlHead :: ( HeadTypeID, HeadID )
    , wlFun :: RefDigest -> IO ()
    }

watchListAdd :: HeadTypeID -> HeadID -> (RefDigest -> IO ()) -> WatchList -> ( WatchList, WatchID )
watchListAdd tid hid cb wl = ( wl', wlNext wl )
  where
    wl' = wl
        { wlNext = nextWatchID (wlNext wl)
        , wlList = WatchListItem
            { wlID = wlNext wl
            , wlHead = (tid, hid)
            , wlFun = cb
            } : wlList wl
        }

watchListDel :: WatchID -> WatchList -> WatchList
watchListDel wid wl = wl { wlList = filter ((/= wid) . wlID) $ wlList wl }


foreign import javascript unsafe
    "const req = window.indexedDB.open($1); req.onerror = (event) => { console.log(\"Error loading database.\"); }; req.onsuccess = (event) => { $2(req.result); }; req.onupgradeneeded = (event) => { const db = event.target.result; db.createObjectStore(\"objects\"); db.createObjectStore(\"heads\"); db.createObjectStore(\"keys\"); }"
    js_indexedDB_open :: JSString -> JSVal -> IO ()


foreign import javascript unsafe
    "const tr = $1.transaction($2, \"readwrite\"); tr.onerror = (event) => { console.log(\"put transaction error\"); }; tr.objectStore($2).put(new Uint8Array(globalThis.wasi_memory.buffer, $4, $5), $3); tr.commit();"
    js_db_put :: JSVal -> JSString -> JSString -> Ptr Word8 -> Int -> IO ()

foreign import javascript unsafe
    "const tr = $1.transaction($2, \"readonly\"); tr.onerror = (event) => { console.log(\"get transaction error\"); }; const req = tr.objectStore($2).get($3); req.onsuccess = $4; tr.commit();"
    js_db_get :: JSVal -> JSString -> JSString -> JSVal -> IO ()

foreign import javascript unsafe
    "const tr = $1.transaction($2, \"readwrite\"); tr.onerror = (event) => { console.log(\"delete transaction error\"); }; const req = tr.objectStore($2).delete($3); tr.commit();"
    js_db_delete :: JSVal -> JSString -> JSString -> IO ()

foreign import javascript unsafe "$1.target.result"
    js_get_targetResult :: JSVal -> IO JSVal


foreign import javascript unsafe "if (!$1) { return 0; }; return $1.byteLength"
    js_get_byteLength :: JSVal -> IO Int

foreign import javascript unsafe "new Uint8Array(globalThis.wasi_memory.buffer, $1, $2.byteLength).set(new Uint8Array($2))"
    js_copyBytes :: Ptr Word8 -> JSVal -> IO ()
