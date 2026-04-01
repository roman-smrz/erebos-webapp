module Version (
    versionLine,
) where

import Paths_erebos_webapp (version)
import Data.Version (showVersion)
import Version.Git

{-# NOINLINE versionLine #-}
versionLine :: String
versionLine = do
    let ver = case gitVersion of
            Just gver
                -> "git " <> gver
            _   -> "v" <> showVersion version
     in ver
