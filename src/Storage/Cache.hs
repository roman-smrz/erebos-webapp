module Storage.Cache (
    cacheStorage,
) where

import Control.Concurrent

import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Function
import Data.Map (Map)
import Data.Map qualified as M

import Erebos.Object
import Erebos.Storage.Backend
import Erebos.Storage.Head

import Storage.WatchList


data CacheStorage = StorageCache
    { cacheParent :: Storage
    , cacheHeads :: MVar [ (( HeadTypeID, HeadID ), RefDigest ) ]
    , cacheObjs :: MVar (Map RefDigest BL.ByteString)
    , cacheKeys :: MVar (Map RefDigest B.ByteString)
    , cacheWatchers :: MVar WatchList
    }

instance Eq CacheStorage where
    (==) = (==) `on` cacheObjs

instance Show CacheStorage where
    show StorageCache {} = "cache"

instance StorageBackend CacheStorage where
    backendLoadBytes StorageCache {..} dgst =
        (M.lookup dgst <$> readMVar cacheObjs) >>= \case
            x@Just {} -> return x
            Nothing -> withStorageBackend cacheParent $ \bck -> do
                backendLoadBytes bck dgst >>= \case
                    x@(Just raw) -> do
                        modifyMVar_ cacheObjs (return . M.insert dgst raw)
                        return x
                    Nothing -> return Nothing

    backendStoreBytes StorageCache {..} dgst raw = do
        modifyMVar_ cacheObjs (return . M.insert dgst raw)
        withStorageBackend cacheParent $ \bck ->
            backendStoreBytes bck dgst raw


    backendLoadHeads StorageCache {..} tid = do
        withStorageBackend cacheParent $ \bck ->
            backendLoadHeads bck tid

    backendLoadHead StorageCache {..} tid hid =
        withStorageBackend cacheParent $ \bck ->
            backendLoadHead bck tid hid

    backendStoreHead StorageCache {..} tid hid dgst =
        withStorageBackend cacheParent $ \bck ->
            backendStoreHead bck tid hid dgst

    backendReplaceHead StorageCache {..} tid hid expected new = do
        withStorageBackend cacheParent $ \bck ->
            backendReplaceHead bck tid hid expected new

    backendWatchHead StorageCache {..} tid hid cb = do
        withStorageBackend cacheParent $ \bck ->
            backendWatchHead bck tid hid cb

    backendUnwatchHead StorageCache {..} wid = do
        withStorageBackend cacheParent $ \bck ->
            backendUnwatchHead bck wid


    backendListKeys StorageCache {..} = do
        withStorageBackend cacheParent $ \bck ->
            backendListKeys bck
    backendLoadKey StorageCache {..} dgst = do
        withStorageBackend cacheParent $ \bck ->
            backendLoadKey bck dgst
    backendStoreKey StorageCache {..} dgst key = do
        withStorageBackend cacheParent $ \bck ->
            backendStoreKey bck dgst key
    backendRemoveKey StorageCache {..} dgst = do
        withStorageBackend cacheParent $ \bck ->
            backendRemoveKey bck dgst


cacheStorage :: Storage -> IO Storage
cacheStorage cacheParent = do
    cacheHeads <- newMVar []
    cacheObjs <- newMVar M.empty
    cacheKeys <- newMVar M.empty
    cacheWatchers <- newMVar (WatchList startWatchID [])
    newStorage $ StorageCache {..}
