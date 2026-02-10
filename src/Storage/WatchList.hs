module Storage.WatchList (
    WatchList(..),
    WatchListItem(..),
    watchListAdd,
    watchListDel,
) where

import Erebos.Object
import Erebos.Storage.Backend
import Erebos.Storage.Head


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
